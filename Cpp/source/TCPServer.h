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
 * File:   TCPServer.h
 * Author: Anton Gravestam
 *
 * Created on den 22 oktober 2008, 20:01
 */

#ifndef ops_TCPServerH
#define	ops_TCPServerH

#include "TCPServerBase.h"
#include "IOService.h" 
#include "BytesSizePair.h"

#include <iostream>
#include <boost/asio.hpp>
#include <boost/array.hpp>
#include "BoostIOServiceImpl.h"
#include "boost/bind.hpp"
#include "Participant.h"
#include "BasicError.h"
#include "BasicWarning.h"
#include "Compatibility.h"

namespace ops
{
    ///Interface used to send data

	class TCPServer : public TCPServerBase
    {
    public:
		TCPServer(IOService* ioServ, Address_T serverIP, int serverPort, int64_t outSocketBufferSize = 16000000) :
			TCPServerBase(),
			_serverPort(serverPort), _serverIP(serverIP), _outSocketBufferSize(outSocketBufferSize),
			endpoint(NULL), sock(NULL), acceptor(NULL), _canceled(false),
			_asyncCallActive(false), _working(false)
		{
			ioService = dynamic_cast<BoostIOServiceImpl*>(ioServ)->boostIOService;
			//boost::asio::ip::address ipAddr(boost::asio::ip::address_v4::from_string(serverIP));
			endpoint = new boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), serverPort);
			sock = new boost::asio::ip::tcp::socket(*ioService);
		}
		
		virtual ~TCPServer()
		{
			close();

			/// We must handle asynchronous callbacks that haven't finished yet.
			/// This approach works, but the recommended boost way is to use a shared pointer to the instance object
			/// between the "normal" code and the callbacks, so the callbacks can check if the object exists.
			while (_working) {
				Sleep(1);
			}

			if (acceptor) delete acceptor;
			if (sock) delete sock;
			if (endpoint) delete endpoint;
		}

		void open()
		{
			_canceled = false;
			if (acceptor) delete acceptor;
			// This constructor opens, binds and listens to the given endpoint.
			acceptor = new boost::asio::ip::tcp::acceptor(*ioService, *endpoint);
			// Set variables indicating that we are "active"
			_working = true;
			_asyncCallActive = true;
			acceptor->async_accept(*sock, boost::bind(&TCPServer::handleAccept, this, boost::asio::placeholders::error));
		}

		void close()
		{
			_canceled = true;
			if (acceptor) acceptor->close();
			TCPServerBase::close();
		}

		virtual int getPort()
		{
			return _serverPort;
		}
        
		virtual Address_T getAddress()
		{
			return _serverIP;
		}

    private:
		class boostSocketSender : public SocketSender
		{
		private:
			boost::asio::ip::tcp::socket* _sock;
		public:
			explicit boostSocketSender(boost::asio::ip::tcp::socket* sock) : _sock(sock) {}
			virtual ~boostSocketSender()
			{
				_sock->close();
				delete _sock;
			}
			virtual int send(char* buf, int size)
			{
				try {
					// Send the data
					return (int)_sock->send(boost::asio::buffer(buf, size));
				} catch (std::exception& e) {
					ErrorMessage_T msg("Socket closed, exception in TCPServer::sendTo():");
					msg += e.what();
					ops::BasicError err("TCPServer", "TCPServer", msg);
					Participant::reportStaticError(&err);
				}
				return -1;
			}
		};

		void handleAccept(const boost::system::error_code& error)
		{
			_asyncCallActive = false;

			if (_canceled) {
				//This TCPServer is shutting down

			} else {
				if (!error) {
					if (_outSocketBufferSize > 0) {
						boost::asio::socket_base::send_buffer_size option((int)_outSocketBufferSize);
						boost::system::error_code ec;
						ec = sock->set_option(option, ec);
						sock->get_option(option);
						if(ec != 0 || option.value() != _outSocketBufferSize) {
							//std::cout << "Socket buffer size could not be set" << std::endl;
							ops::BasicWarning err("TCPServer", "TCPServer", "Socket buffer size could not be set");
							Participant::reportStaticError(&err);
						}
					}

					//Disable Nagle algorithm
					boost::asio::ip::tcp::no_delay option(true);
					sock->set_option(option);

					AddSocket(new boostSocketSender(sock));

					sock = new boost::asio::ip::tcp::socket(*ioService);
					_asyncCallActive = true;
					acceptor->async_accept(*sock, boost::bind(&TCPServer::handleAccept, this, boost::asio::placeholders::error));

				} else {
					_asyncCallActive = true;
					acceptor->async_accept(*sock, boost::bind(&TCPServer::handleAccept, this, boost::asio::placeholders::error));
				}
			}
			// We update the "m_working" flag as the last thing in the callback, so we don't access the object any more 
			// in case the destructor has been called and waiting for us to be finished.
			// If we haven't started a new async call above, this will clear the flag.
			_working = _asyncCallActive;
		}

		int _serverPort;
		Address_T _serverIP;
		int64_t _outSocketBufferSize;
		boost::asio::ip::tcp::endpoint* endpoint;		//<-- The local port to bind to.
        boost::asio::ip::tcp::socket* sock;				//<-- The socket that handles next accept.
        
		boost::asio::ip::tcp::acceptor* acceptor;
		boost::asio::io_service* ioService;				//<-- Boost io_service handles the asynhronous operations on the sockets

		bool _canceled;

		// Variables to keep track of our outstanding requests, that will result in callbacks to us
		volatile bool _asyncCallActive;
		volatile bool _working;
    };
}
#endif

