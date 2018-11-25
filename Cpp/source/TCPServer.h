/**
* 
* Copyright (C) 2006-2009 Anton Gravestam.
* Copyright (C) 2018 Lennart Andersson.
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

#pragma once

#include <iostream>
#include <boost/asio.hpp>
#include <boost/array.hpp>
#include <boost/bind.hpp>

#include "TCPServerBase.h"
#include "IOService.h" 
#include "BytesSizePair.h"
#include "BoostIOServiceImpl.h"
#include "Participant.h"
#include "BasicError.h"
#include "BasicWarning.h"
#include "Compatibility.h"
#include "TCPBoostConnection.h"

namespace ops
{
    ///Interface used to send data

	class TCPServer : public TCPServerBase
    {
    public:
		TCPServer(TCPServerCallbacks* client, IOService* ioServ, Address_T serverIP, int serverPort, TCPProtocol* protocol, int64_t outSocketBufferSize = 16000000) :
			TCPServerBase(client, protocol),
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

		void open() override
		{
			/// We must handle asynchronous callbacks that haven't finished yet (in case open()/close() called multiple times)
			/// We must not delete the acceptor while the async call is active.
			while (_working) {
				Sleep(1);
			}
			_canceled = false;
			if (acceptor) delete acceptor;

			// This constructor opens, sets reuse_address, binds and listens to the given endpoint.
			acceptor = new boost::asio::ip::tcp::acceptor(*ioService, *endpoint);
			// Set variables indicating that we are "active"
			_working = true;
			_asyncCallActive = true;
			acceptor->async_accept(*sock, boost::bind(&TCPServer::handleAccept, this, boost::asio::placeholders::error));
		}

		void close() override
		{
			_canceled = true;
			if (acceptor) acceptor->close();
			TCPServerBase::close();
		}

		int getLocalPort() override
		{
			return _serverPort;
		}
        
		Address_T getLocalAddress() override
		{
			return _serverIP;
		}

    private:
		void handleAccept(const boost::system::error_code& error)
		{
			_asyncCallActive = false;

			if (_canceled) {
				//This TCPServer is shutting down

			} else {
				if (!error) {
					AddSocket(new TCPBoostConnection(this, sock, _outSocketBufferSize));
					sock = new boost::asio::ip::tcp::socket(*ioService);
				}
				_asyncCallActive = true;
				acceptor->async_accept(*sock, boost::bind(&TCPServer::handleAccept, this, boost::asio::placeholders::error));
			}
			// We update the "_working" flag as the last thing in the callback, so we don't access the object any more 
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

