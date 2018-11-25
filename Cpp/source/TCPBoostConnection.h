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

#include "OPSTypeDefs.h"
#include "BasicError.h"
#include "BasicWarning.h"
#include "TCPConnection.h"

namespace ops
{
	// Helper class for a connected TCP socket
	class TCPBoostConnection : public TCPConnection
	{
	protected:
		boost::asio::ip::tcp::socket* _sock;
		volatile bool _connected;
		// Variables to keep track of our outstanding requests, that will result in callbacks to us
		volatile bool _asyncCallActive;
		volatile bool _working;

		// Used for a connection created by TCPClient
		TCPBoostConnection(TCPConnectionCallbacks* client) :
			TCPConnection(client),
			_sock(nullptr), _connected(false), _asyncCallActive(false), _working(false)
		{
		}

	public:
		// Used for connections created by TCPServer
		// In this case the socket is already connected
		explicit TCPBoostConnection(TCPConnectionCallbacks* client, boost::asio::ip::tcp::socket* sock, int64_t outSocketBufferSize) :
			TCPConnection(client),
			_sock(sock), _connected(true), _asyncCallActive(false), _working(false)
		{
			setOutSize(outSocketBufferSize);
			disableNagleAlg();
		}

		virtual ~TCPBoostConnection()
		{
			if (_sock) _sock->close();

			/// We must handle asynchronous callbacks that haven't finished yet.
			/// This approach works, but the recommended boost way is to use a shared pointer to the instance object
			/// between the "normal" code and the callbacks, so the callbacks can check if the object exists.
			while (_working) {
				Sleep(1);
			}

			if (_sock) delete _sock;
		}

		void close() override
		{
			if (_sock) _sock->close();
		}

		void setOutSize(int64_t size)
		{
			if (_sock && (size > 0)) {
				boost::asio::socket_base::send_buffer_size option((int)size);
				boost::system::error_code ec;
				ec = _sock->set_option(option, ec);
				_sock->get_option(option);
				if (ec || option.value() != size) {
					BasicWarning err("TCPBoostConnection", "setOutSize", "Socket buffer size could not be set");
					Participant::reportStaticError(&err);
				}
			}

		}

		void setInSize(int64_t size)
		{
			if (_sock && (size > 0)) {
				boost::asio::socket_base::receive_buffer_size option((int)size);
				boost::system::error_code ec;
				ec = _sock->set_option(option, ec);
				_sock->get_option(option);
				if (ec || option.value() != size) {
					ops::BasicWarning err("TCPBoostConnection", "setInSize", "Socket buffer size could not be set");
					Participant::reportStaticError(&err);
				}
			}
		}

		//Disable Nagle algorithm
		void disableNagleAlg()
		{
			if (_sock) {
				boost::asio::ip::tcp::no_delay option(true);
				boost::system::error_code error;
				_sock->set_option(option, error);
				if (error) {
					ops::BasicWarning warn("TCPBoostConnection", "disableNagleAlg", "Failed to disable Nagle algorithm.");
					Participant::reportStaticError(&warn);
				}
			}
		}

		bool isConnected() override
		{
			return _connected;
		}

		// Returns true if all asynchronous work has finished
		bool asyncFinished() override
		{
			return !_working;
		}

		void startAsyncRead(char* bytes, int size) override
		{
			if (_connected) {
				// Set variables indicating that we are "active"
				_working = true;
				_asyncCallActive = true;
				_sock->async_receive(
					boost::asio::buffer(bytes, size),
					boost::bind(&TCPBoostConnection::handle_receive_data, this, boost::asio::placeholders::error, boost::asio::placeholders::bytes_transferred));
			}
		}

		void handle_receive_data(const boost::system::error_code& error, size_t nrBytesReceived)
		{
			_asyncCallActive = false;
			if (_connected) {
				handleReceivedData(error.value(), (int)nrBytesReceived);
			}
			// We update the "_working" flag as the last thing in the callback, so we don't access the object any more
			// in case the destructor has been called and waiting for us to be finished.
			// If we haven't started a new async call above, this will clear the flag.
			_working = _asyncCallActive;
		}

		int send(char* buf, int size) override
		{
			try {
				// Send the data
				return (int)_sock->send(boost::asio::buffer(buf, size));
			} catch (std::exception& e) {
				ErrorMessage_T msg("Exception: ");
				msg += e.what();
				BasicError err("TCPBoostConnection", "send", msg);
				Participant::reportStaticError(&err);
			}
			return -1;
		}

		void getRemote(Address_T& address, int& port) override
		{
			boost::system::error_code error;
			boost::asio::ip::tcp::endpoint sendingEndPoint;
			sendingEndPoint = _sock->remote_endpoint(error);
			address = sendingEndPoint.address().to_string().c_str();
			port = sendingEndPoint.port();
		}

		void getLocal(Address_T& address, int& port) override
		{
			boost::system::error_code error;
			boost::asio::ip::tcp::endpoint localEndPoint;
			localEndPoint = _sock->local_endpoint(error);
			address = localEndPoint.address().to_string().c_str();
			port = localEndPoint.port();
		}
	};
}
