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

#pragma once

#include <boost/asio.hpp>
#include <boost/array.hpp>
#include <boost/bind.hpp>

#include "TCPClientBase.h"
#include "IOService.h"
#include "BoostIOServiceImpl.h"
#include "Participant.h"
#include "BasicError.h"
#include "BasicWarning.h"
#include "Compatibility.h"
#include "TCPBoostConnection.h"

namespace ops
{

    class TCPClient : public TCPClientBase
    {
		int64_t inSocketBufferSizent;

		// Internal helper class that handles the socket connection
		class TCPBoostConnectionWConnect : public TCPBoostConnection
		{
			TCPClient* _owner;
			volatile bool _tryToConnect;
			boost::asio::ip::tcp::endpoint* _endpoint;
			int64_t _inBufferSize;

		public:
			TCPBoostConnectionWConnect(TCPClient* owner, Address_T serverIP, int serverPort, IOService* ioServ, int64_t inBufferSize) :
				TCPBoostConnection(owner),
				_owner(owner), _tryToConnect(false), _endpoint(nullptr), _inBufferSize(inBufferSize)
			{
				boost::asio::io_service* ioService = dynamic_cast<BoostIOServiceImpl*>(ioServ)->boostIOService;
				boost::asio::ip::address ipAddr(boost::asio::ip::address_v4::from_string(serverIP.c_str()));
				_endpoint = new boost::asio::ip::tcp::endpoint(ipAddr, serverPort);
				_sock = new boost::asio::ip::tcp::socket(*ioService);
			}

			~TCPBoostConnectionWConnect()
			{
				if (_endpoint) delete _endpoint;
			}

			void start()
			{
				_tryToConnect = true;
				_connected = false;
				// Set variables indicating that we are "active"
				_working = true;
				_asyncCallActive = true;
				_sock->async_connect(*_endpoint, boost::bind(&TCPBoostConnectionWConnect::handleConnect, this, boost::asio::placeholders::error));
			}

			void stop()
			{
				if (_tryToConnect) {
					//Close the socket 
					_tryToConnect = false;
					_connected = false;
					if (_sock) _sock->close();
					_owner->connected(false);
				}
			}

			void handleConnect(const boost::system::error_code& error)
			{
				_asyncCallActive = false;
				try {
					if (_tryToConnect) {
						if (error) {
							_owner->connected(false);
							//connect again
							_connected = false;
							_asyncCallActive = true;
							_sock->async_connect(*_endpoint, boost::bind(&TCPBoostConnectionWConnect::handleConnect, this, boost::asio::placeholders::error));

						} else {
							_connected = true;

							setInSize(_inBufferSize);
							disableNagleAlg();

							_owner->connected(true);
						}
					}
				} catch (std::exception& e) {
					ExceptionMessage_T msg("Unknown exception: ");
					msg += e.what();
					ops::BasicWarning err("TCPClient", "handleConnect", msg);
					Participant::reportStaticError(&err);
				}
				// We update the "_working" flag as the last thing in the callback, so we don't access the object any more
				// in case the destructor has been called and waiting for us to be finished.
				// If we haven't started a new async call above, this will clear the flag.
				_working = _asyncCallActive;
			}
		};

	public:
        TCPClient(Address_T serverIP, int serverPort, IOService* ioServ, TCPProtocol* protocol, int64_t inSocketBufferSizent = 16000000) :
			TCPClientBase(protocol, new TCPBoostConnectionWConnect(this, serverIP, serverPort, ioServ, inSocketBufferSizent))
        {}

		void start() override
		{
			dynamic_cast<TCPBoostConnectionWConnect*>(_connection)->start();
		}

        void stop() override
        {
			dynamic_cast<TCPBoostConnectionWConnect*>(_connection)->stop();
		}

		// Used to get the sender IP and port for a received message
		// Only safe to call in callback, before a new asynchWait() is called.
		void getSource(Address_T& address, int& port) override
		{
			_connection->getRemote(address, port);
		}

		// Returns true if all asynchronous work has finished
		virtual bool asyncFinished() override
		{
			return _connection->asyncFinished();
		}

		virtual ~TCPClient()
		{
			// Make sure socket is closed
			stop();
			delete _connection;
        }

		bool isConnected() override
		{ 
			return _connection->isConnected();
		}

		int getLocalPort() override
		{
			Address_T address;
			int port;
			_connection->getLocal(address, port);
			return port;
		}

		Address_T getLocalAddress() override
		{
			Address_T address;
			int port;
			_connection->getLocal(address, port);
			return address;
		}

    };
}
