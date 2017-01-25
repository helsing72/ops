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

/* 
 * File:   Transport.h
 * Author: Anton Gravestam
 *
 * Created on den 22 oktober 2008, 20:01
 */

#ifndef ops_TCPServerH
#define	ops_TCPServerH

#include <string>
#include "Sender.h"
#include "IOService.h" 
#include "BytesSizePair.h"

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
    ///Interface used to send data

	class TCPServer : public Sender
    {
    public:
		TCPServer(IOService* ioServ, std::string serverIP, int serverPort, __int64 outSocketBufferSize = 16000000) :
			endpoint(NULL), sock(NULL), acceptor(NULL), connected(false), canceled(false),
			m_asyncCallActive(false), m_working(false)
		{
			this->outSocketBufferSize = outSocketBufferSize;
			ioService = ((BoostIOServiceImpl*)ioServ)->boostIOService;
			//boost::asio::ip::address ipAddr(boost::asio::ip::address_v4::from_string(serverIP));
			endpoint = new boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), serverPort);
			sock = new boost::asio::ip::tcp::socket(*ioService);
		}
		
		void open()
		{
			canceled = false;
			if (acceptor) delete acceptor;
			// This constructor opens, binds and listens to the given endpoint.
			acceptor = new boost::asio::ip::tcp::acceptor(*ioService, *endpoint);
			// Set variables indicating that we are "active"
			m_working = true;
			m_asyncCallActive = true;
			acceptor->async_accept(*sock, boost::bind(&TCPServer::handleAccept, this, boost::asio::placeholders::error));
		}

		///Sends buf to any Receiver connected to this Sender, ip and port are discarded and can be left blank.
        virtual bool sendTo(char* buf, int size, const std::string& ip, int port)
		{
			//Send to anyone connected. Loop backwards to avoid problems when removing broken sockets
			for(int i = (int)connectedSockets.size() - 1; i >= 0 ; i--)
			{
				try
				{
					//First, prepare and send a package of fixed length 22 with information about the size of the data package
					char sizeInfo[100] = "opsp_tcp_size_info"; 
					memcpy(sizeInfo + 18, (void*)&size, 4);
					connectedSockets[i]->send(boost::asio::buffer(sizeInfo, 22));

					//Send the actual data
					connectedSockets[i]->send(boost::asio::buffer(buf, size));

				}
				catch(std::exception& e)
				{
					std::stringstream ss;
					ss << "Socket closed, exception in TCPServer::sendTo():" << e.what() << std::endl;
					ops::BasicError err("TCPServer", "TCPServer", ss.str());
					Participant::reportStaticError(&err);
					connectedSockets[i]->close();

					std::vector<boost::asio::ip::tcp::socket*>::iterator it;
					it = connectedSockets.begin();
					it += i;
					delete connectedSockets[i];
					connectedSockets.erase(it);
					
					connected = (connectedSockets.size() > 0);
				}
			}
			return true;
		}
		//virtual int receiveReply(char* buf, int size)
		//{
		//	return -9999999;
		//}
        virtual int getPort()
		{
			return port;
		}
        virtual std::string getAddress()
		{
			return ipAddress;
		}

        virtual ~TCPServer()
        {
			close();

			/// We must handle asynchronous callbacks that haven't finished yet.
			/// This approach works, but the recommended boost way is to use a shared pointer to the instance object
			/// between the "normal" code and the callbacks, so the callbacks can check if the object exists.
			while (m_working) { 
				Sleep(1);
			}

			if (acceptor) delete acceptor;
			if (sock) delete sock;
			if (endpoint) delete endpoint;
		}

		void close()
		{
			canceled = true;
			if (acceptor) acceptor->close();
			for(int i = (int)connectedSockets.size() - 1; i >= 0 ; i--)
			{
				connectedSockets[i]->close();
				delete connectedSockets[i];
			}
			connectedSockets.clear();
			connected = false;
		}

    private:
		void handleAccept(const boost::system::error_code& error)
		{
			m_asyncCallActive = false;

			if (canceled) {
				//This TCPServer is shutting down

			} else {
				if(!error)
				{
					if (outSocketBufferSize > 0) {
						boost::asio::socket_base::send_buffer_size option((int)outSocketBufferSize);
						boost::system::error_code ec;
						ec = sock->set_option(option, ec);
						sock->get_option(option);
						if(ec != 0 || option.value() != outSocketBufferSize)
						{
							//std::cout << "Socket buffer size could not be set" << std::endl;
							ops::BasicError err("TCPServer", "TCPServer", "Socket buffer size could not be set");
							Participant::reportStaticError(&err);
						}
					}
	//				std::cout << "accept ok" << std::endl;
					connectedSockets.push_back(sock);
					sock = new boost::asio::ip::tcp::socket(*ioService);
					m_asyncCallActive = true;
					acceptor->async_accept(*sock, boost::bind(&TCPServer::handleAccept, this, boost::asio::placeholders::error));
					connected = true;
				}
				else
				{
	//				std::cout << "accept failed" << std::endl;
					m_asyncCallActive = true;
					acceptor->async_accept(*sock, boost::bind(&TCPServer::handleAccept, this, boost::asio::placeholders::error));
				}
			}
			// We update the "m_working" flag as the last thing in the callback, so we don't access the object any more 
			// in case the destructor has been called and waiting for us to be finished.
			// If we haven't started a new async call above, this will clear the flag.
			m_working = m_asyncCallActive;
		}

		int port;
		std::string ipAddress;
		__int64 outSocketBufferSize;
		boost::asio::ip::tcp::endpoint* endpoint;		//<-- The local port to bind to.
        boost::asio::ip::tcp::socket* sock;				//<-- The socket that handles next accept.
        
		boost::asio::ip::tcp::acceptor* acceptor;
		bool connected;
		std::vector<boost::asio::ip::tcp::socket*> connectedSockets; //<-- All connected sockets
		boost::asio::io_service* ioService;				//<-- Boost io_service handles the asynhronous operations on the sockets

		bool canceled;

		// Variables to keep track of our outstanding requests, that will result in callbacks to us
		volatile bool m_asyncCallActive;
		volatile bool m_working;
    };
}



#endif	/* ops_TCPServerH */

