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

#ifndef ops_TCPCLientH
#define ops_TCPClientH

#include <string>
#include "TCPClientBase.h"
#include "IOService.h" 

#include <iostream>
#include <boost/asio.hpp>
#include <boost/array.hpp>
#include "BoostIOServiceImpl.h"
#include "boost/bind.hpp"
#include "Participant.h"
#include "BasicError.h"
#include "Compatibility.h"

namespace ops
{

    class TCPClient : public TCPClientBase
    {
    public:
        TCPClient(std::string serverIP, int serverPort, IOService* ioServ, __int64 inSocketBufferSizent = 16000000) : 
			_serverPort(serverPort), ipaddress(serverIP),
			sock(NULL), endpoint(NULL), _connected(false), 
			tryToConnect(false),
			m_asyncCallActive(false), m_working(false)
        {
            boost::asio::io_service* ioService = ((BoostIOServiceImpl*) ioServ)->boostIOService;
            boost::asio::ip::address ipAddr(boost::asio::ip::address_v4::from_string(serverIP));
            endpoint = new boost::asio::ip::tcp::endpoint(ipAddr, serverPort);
			this->inSocketBufferSizent = inSocketBufferSizent;

            sock = new boost::asio::ip::tcp::socket(*ioService);
        }

		void start()
		{
			tryToConnect = true;
            _connected = false;
			// Set variables indicating that we are "active"
			m_working = true;
			m_asyncCallActive = true;
            sock->async_connect(*endpoint, boost::bind(&TCPClient::handleConnect, this, boost::asio::placeholders::error));
		}

        void stop()
        {
			//Close the socket 
			tryToConnect = false;
            _connected = false;
            if (sock) sock->close();
			connected(false);
		}

        void handleConnect(const boost::system::error_code& error)
        {
			m_asyncCallActive = false;
			if (tryToConnect) {
				if (error) {
					connected(false);
					//connect again
					_connected = false;
					//std::cout << "connection failed tcp asynch" << std::endl;
///					ops::BasicError err("TCPClient", "handleConnect", "connection failed tcp asynch");
///LA too much output        Participant::reportStaticError(&err);
					m_asyncCallActive = true;
					sock->async_connect(*endpoint, boost::bind(&TCPClient::handleConnect, this, boost::asio::placeholders::error));
				}
				else
				{
					_connected = true;

					if(inSocketBufferSizent > 0)
					{
						boost::asio::socket_base::receive_buffer_size option((int)inSocketBufferSizent);
						boost::system::error_code ec;
						ec = sock->set_option(option, ec);
						sock->get_option(option);
						if (ec != 0 || option.value() != inSocketBufferSizent)
						{
							//std::cout << "Socket buffer size could not be set" << std::endl;
							ops::BasicError err("TCPClient", "TCPClient", "Socket buffer size could not be set");
							Participant::reportStaticError(&err);
						}
					}

					//Disable Nagle algorithm

					boost::asio::ip::tcp::no_delay option2(true);
					sock->set_option(option2);
					//if(sockOptErr != 0)
					//{
					//std::cout << "Failed to disable Nagle algorithm." << std::endl;
					//ops::BasicError err("Failed to disable Nagle algorithm.");
					//Participant::reportStaticError(&err);
					//}

					//  std::cout << "connected tcp asynch" << std::endl;
					connected(true);
				}
			}
			// We update the "m_working" flag as the last thing in the callback, so we don't access the object any more 
			// in case the destructor has been called and waiting for us to be finished.
			// If we haven't started a new async call above, this will clear the flag.
			m_working = m_asyncCallActive;
        }

		// Used to get the sender IP and port for a received message
		// Only safe to call in callback, before a new asynchWait() is called.
		void getSource(std::string& address, int& port) 
		{
		    boost::system::error_code error;
			boost::asio::ip::tcp::endpoint sendingEndPoint;
			sendingEndPoint = sock->remote_endpoint( error );
			address = sendingEndPoint.address().to_string();
			port = sendingEndPoint.port();
		}

        virtual ~TCPClient()
        {
			// Make sure socket is closed
			stop();

			/// We must handle asynchronous callbacks that haven't finished yet.
			/// This approach works, but the recommended boost way is to use a shared pointer to the instance object
			/// between the "normal" code and the callbacks, so the callbacks can check if the object exists.
			while (m_working) { 
				Sleep(1);
			}

			if (sock) delete sock;
            if (endpoint) delete endpoint;
        }

		void startAsyncRead(char* bytes, int size)
        {
            if (_connected) {
				// Set variables indicating that we are "active"
				m_working = true;
				m_asyncCallActive = true;
                sock->async_receive(
                        boost::asio::buffer(bytes, size),
                        boost::bind(&TCPClient::handle_receive_data, this, boost::asio::placeholders::error, boost::asio::placeholders::bytes_transferred));
            }
        }

        void handle_receive_data(const boost::system::error_code& error, size_t nrBytesReceived)
        {
			m_asyncCallActive = false;
			handleReceivedData(error.value(), (int)nrBytesReceived);
			// We update the "m_working" flag as the last thing in the callback, so we don't access the object any more 
			// in case the destructor has been called and waiting for us to be finished.
			// If we haven't started a new async call above, this will clear the flag.
			m_working = m_asyncCallActive;
        }

		bool isConnected() 
		{ 
			return _connected; 
		}

		int getLocalPort()
		{
			return _serverPort;
		}

		std::string getLocalAddress()
		{
			return ipaddress;
		}

	private:
        int _serverPort;
        std::string ipaddress;
		__int64 inSocketBufferSizent;
        boost::asio::ip::tcp::socket* sock;
        boost::asio::ip::tcp::endpoint* endpoint;

        bool _connected;
		bool tryToConnect;

		// Variables to keep track of our outstanding requests, that will result in callbacks to us
		volatile bool m_asyncCallActive;
		volatile bool m_working;
    };
}
#endif
