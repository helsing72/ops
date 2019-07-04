/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
 *
 * This file is part of OPS (Open Publish Subscribe).
 *
 * OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
 */

#ifndef ops_UDPReceiverH
#define ops_UDPReceiverH

#include "Participant.h"
#include "Receiver.h"
#include <boost/asio.hpp>
#include "boost/bind.hpp"
#include "ByteBuffer.h"
#include "BoostIOServiceImpl.h"
#include <iostream>
#include "BasicError.h"
#include "BasicWarning.h"
#include "Compatibility.h"

namespace ops
{
    using boost::asio::ip::udp;

    class UDPReceiver : public Receiver
    {
    public:

        UDPReceiver(int bindPort, IOService* ioServ, Address_T localInterface = "0.0.0.0", int64_t inSocketBufferSizent = 16000000) :
	        sock(nullptr), localEndpoint(nullptr), max_length(65535), data(nullptr),
			cancelled(false), m_asyncCallActive(false), m_working(false)
        {
            boost::asio::io_service* ioService = dynamic_cast<BoostIOServiceImpl*>(ioServ)->boostIOService;

            if (localInterface == "0.0.0.0")
            {
                udp::resolver resolver(*ioService);
                udp::resolver::query query(boost::asio::ip::host_name(), "");
                udp::resolver::iterator it = resolver.resolve(query);
                udp::resolver::iterator end;
                while (it != end)
                {
                    boost::asio::ip::address addr = it->endpoint().address();
                    if (addr.is_v4())
                    {
                        ipaddress = addr.to_string().c_str();
                        localEndpoint = new udp::endpoint(addr, bindPort);
                        break;
                    }
                    ++it;
                }
            }
            else
            {
                boost::asio::ip::address ipAddr(boost::asio::ip::address_v4::from_string(localInterface.c_str()));
                localEndpoint = new boost::asio::ip::udp::endpoint(ipAddr, bindPort);
				ipaddress = localInterface;
            }

            sock = new boost::asio::ip::udp::socket(*ioService);

            sock->open(localEndpoint->protocol());

            if (inSocketBufferSizent > 0)
            {
                boost::asio::socket_base::receive_buffer_size option((int) inSocketBufferSizent);
                boost::system::error_code ec;
                ec = sock->set_option(option, ec);
                sock->get_option(option);
                if (ec || option.value() != inSocketBufferSizent)
                {
                    //std::cout << "Socket buffer size could not be set" << std::endl;
					ops::BasicWarning err("UDPReceiver", "UDPReceiver", "Socket buffer size could not be set");
                    Participant::reportStaticError(&err);
                }
            }

			try {
	            //Note, takes address even if in use.
		        sock->set_option(boost::asio::ip::udp::socket::reuse_address(true));
			    sock->bind(*localEndpoint);

				//ipaddress = localEndpoint->address().to_string();
	            port = sock->local_endpoint().port();
			} catch (...) {
				ops::BasicError err("UDPReceiver", "UDPReceiver", "Failed to setup UDP socket. Check address");
				Participant::reportStaticError(&err);
				stop();
			}
        }

        void asynchWait(char* bytes, int size)
        {
            data = bytes;
            max_length = size;
			// Set variables indicating that we are "active"
			m_working = true;
			m_asyncCallActive = true;
            sock->async_receive_from(
                    boost::asio::buffer(data, max_length),
					sendingEndPoint,
                    boost::bind(&UDPReceiver::handle_receive_from, this, boost::asio::placeholders::error, boost::asio::placeholders::bytes_transferred));
        }

		// Used to get the sender IP and port for a received message
		// Only safe to call in callback, before a new asynchWait() is called.
		void getSource(Address_T& address, int& port)
		{
			address = sendingEndPoint.address().to_string().c_str();
			port = sendingEndPoint.port();
		}

        void handle_receive_from(const boost::system::error_code& error, size_t nrBytesReceived)
        {
			m_asyncCallActive = false;	// No longer a call active, thou we may start a new one below
			if (!cancelled)
            {
	            if (!error && nrBytesReceived > 0)
		        {
			        handleReadOK(data, (int)nrBytesReceived);
				}
	            else
		        {
			        handleReadError(error);
				}
			}
			// We update the "m_working" flag as the last thing in the callback, so we don't access the object any more 
			// in case the destructor has been called and waiting for us to be finished.
			// If we haven't started a new async call above, this will clear the flag.
			m_working = m_asyncCallActive;
        }

        void handleReadOK(char* bytes_, int size)
        {
            UNUSED(bytes_);
            notifyNewEvent(BytesSizePair(data, size));
        }

        void handleReadError(const boost::system::error_code& error)
        {
            if (error.value() == BREAK_COMM_ERROR_CODE)
            {
                //Communcation has been canceled from stop, do not scedule new receive
                return;
            }

			ops::BasicError err("UDPReceiver", "handleReadError", "Error");
            Participant::reportStaticError(&err);

			//WSAEFAULT (10014) "Illegal buffer address" is fatal, happens e.g. if a too small buffer is given and
			// it probably wont go away by calling the same again, so just report error and then exit without 
			// starting a new async_receive().
#ifdef _WIN32
			if (error.value() == WSAEFAULT) return;
#else
///TODO LINUX			if (error.value() == WSAEFAULT) return;
#endif

			asynchWait(data, max_length);
        }

        ~UDPReceiver()
        {
			// Make sure socket is closed
//#if BOOST_VERSION == 103800
//			boost::system::error_code error(BREAK_COMM_ERROR_CODE, boost::system::generic_category);
//#else
//			boost::system::error_code error(BREAK_COMM_ERROR_CODE, boost::system::generic_category());
//#endif
            cancelled = true;
			if (sock) sock->close();

			/// We must handle asynchronous callbacks that haven't finished yet.
			/// This approach works, but the recommended boost way is to use a shared pointer to the instance object
			/// between the "normal" code and the callbacks, so the callbacks can check if the object exists.
			while (m_working) { 
				Sleep(1);
			}

            if (sock) delete sock;
            if (localEndpoint) delete localEndpoint;
        }

        int receive(char* buf, int size)
        {
            try
            {
                size_t nReceived = sock->receive_from(boost::asio::buffer(buf, size), lastEndpoint);
                return (int)nReceived;
            }
            catch (...)
            {
				ops::BasicError err("UDPReceiver", "receive", "Exception");
                Participant::reportStaticError(&err);
                return -1;
            }
        }

        int available()
        {
            return (int)sock->available();
        }

        bool sendReply(char* buf, int size)
        {
            try
            {
                sock->send_to(boost::asio::buffer(buf, size), lastEndpoint);
                return true;
            }
            catch (...)
            {
                return false;
            }
        }

        int getLocalPort()
        {
            return port;
        }

		Address_T getLocalAddress()
        {
            return ipaddress;
        }

        ///Override from Receiver
		void start()
		{
			/// The UDP Receiver is open the whole life time
            cancelled = false;
		}

        void stop()
        {
			/// The UDP Receiver is open the whole life time
			// Cancel the asynch_receive()
            cancelled = true;
			//sock->cancel(); ///FAILS ON WIN XP
			if (sock) sock->close();

			/// We must handle asynchronous callbacks that haven't finished yet.
			/// This approach works, but the recommended boost way is to use a shared pointer to the instance object
			/// between the "normal" code and the callbacks, so the callbacks can check if the object exists.
			while (m_working) { 
				Sleep(1);
			}
        }

		// Returns true if all asynchronous work has finished
		virtual bool asyncFinished()
		{
			return !m_working;
		}

    private:
        int port;
		Address_T ipaddress;
        boost::asio::ip::udp::socket* sock;
        boost::asio::ip::udp::endpoint* localEndpoint;
        boost::asio::ip::udp::endpoint lastEndpoint;
        //boost::asio::io_service* ioService;

		boost::asio::ip::udp::endpoint sendingEndPoint;

        int max_length; //enum { max_length = 65535 };
        char* data; //[max_length];

        static const int BREAK_COMM_ERROR_CODE = 345676;
        bool cancelled;

		// Variables to keep track of our outstanding requests, that will result in callbacks to us
		volatile bool m_asyncCallActive;
		volatile bool m_working;
    };
}
#endif
