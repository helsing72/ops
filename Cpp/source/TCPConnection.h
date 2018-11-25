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

#include "OPSTypeDefs.h"
#include "TCPProtocol.h"

namespace ops
{
	class TCPConnection;

	struct TCPConnectionCallbacks
	{
		virtual void onEvent(TCPConnection* prot, BytesSizePair arg) = 0;
		virtual void onReceiveError(TCPConnection* prot) = 0;
	};

	// Helper class for a connected TCP socket
	class TCPConnection : TCPProtocolCallbacks
	{
	public:
		TCPConnection(TCPConnectionCallbacks* client) :
			_client(client), _protocol(nullptr)
		{}
		
		virtual ~TCPConnection()
		{
			if (_protocol) delete _protocol;
		}

		void asynchWait(char* bytes, int size)
		{
			if (isConnected()) {
				_protocol->startReceive(bytes, size);
			}
		}

		int sendData(char* buf, int size)
		{
			return _protocol->sendData(buf, size);
		}

		// Returns true if all asynchronous work has finished
		virtual bool asyncFinished() = 0;

		virtual void getLocal(Address_T& address, int& port) = 0;
		virtual void getRemote(Address_T& address, int& port) = 0;

		virtual void close() = 0;

		virtual bool isConnected() = 0;

	protected:
		virtual int send(char* buf, int size) = 0;
		virtual void startAsyncRead(char* bytes, int size) = 0;

		// Should be called by the derived classes
		void handleReceivedData(int error, int nrBytesReceived)
		{
			if (!_protocol->handleReceivedData(error, nrBytesReceived)) {
				// If we come here the protocol can't receive any longer and we need to disconnect
				_client->onReceiveError(this);
			}
		}

	private:
		friend class TCPServerBase;
		friend class TCPClientBase;

		TCPConnectionCallbacks* _client;
		TCPProtocol* _protocol;

		void setProtocol(TCPProtocol* prot)
		{
			_protocol = prot;
			_protocol->connect(this);
		}

		// Needed by protocol
		bool isConnected(TCPProtocol* prot) override
		{
			UNUSED(prot);
			return isConnected();
		}

		// Needed by protocol
		void startAsyncRead(TCPProtocol* prot, char* bytes, int size) override
		{
			UNUSED(prot);
			startAsyncRead(bytes, size);
		}

		// Needed by protocol
		void onEvent(TCPProtocol* prot, BytesSizePair arg) override
		{
			UNUSED(prot);
			_client->onEvent(this, arg);
		}

		// Called from protocol
		virtual int sendBuffer(TCPProtocol* prot, char* bytes, int size) override
		{
			UNUSED(prot);
			return send(bytes, size);
		}
	};
}
