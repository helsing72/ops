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
 * File:   TCPServerBase.h
 * Author: Anton Gravestam
 *
 * Created on den 22 oktober 2008, 20:01
 */

#pragma once

#include <vector>

#include "OPSTypeDefs.h"

#include "Notifier.h"
#include "Lockable.h"
#include "Sender.h"
#include "ConnectStatus.h"
#include "TCPProtocol.h"
#include "TCPConnection.h"

namespace ops
{
	struct TCPServerCallbacks
	{
		// Called from server when a new connection is accepted
		// Could be used to call conn->asynchRead(buffer, size)
		virtual void onConnect(TCPConnection* conn, ConnectStatus status) = 0;

		// Called from server when data has been filled into given buffer
		// A new call to conn->asynchRead(buffer, size) need to be done to continue to read
		virtual void onEvent(TCPConnection* conn, BytesSizePair arg) = 0;

		// Called from server when a connection has been deleted
		// NOTE: 'conn' is invalid and is only provided as an ID.
		// Ev. buffer used in asynchRead() is no longer in use
		virtual void onDisconnect(TCPConnection* conn, ConnectStatus status) = 0;
	};

	class TCPServerBase : public Sender, protected TCPConnectionCallbacks
    {
    public:
		TCPServerBase(TCPServerCallbacks* client, TCPProtocol* protocol) :
			_client(client), _protocol(protocol)
		{}

		virtual ~TCPServerBase()
		{
			close();
			delete _protocol;
		}

		void close() override
		{
			SafeLock lck(&_mtx);
			for (int i = (int)_connectedSockets.size() - 1; i >= 0; i--) {
				ConnectStatus st(false, (int)_connectedSockets.size() - 1);
				_connectedSockets[i]->getRemote(st.addr, st.port);
				delete _connectedSockets[i];
				_client->onDisconnect(_connectedSockets[i], st);
			}
			_connectedSockets.clear();
		}

		///Sends buf to all Receivers connected to this Sender, ip and port are discarded and can be left blank.
        bool sendTo(char* buf, int size, const Address_T& ip, int port) override
		{
			UNUSED(ip);
			UNUSED(port);
			SafeLock lck(&_mtx);
			// Send to anyone connected. Loop backwards to avoid problems when removing broken sockets
			for (int i = (int)_connectedSockets.size() - 1; i >= 0 ; i--) {
				// Send the data
				if (_connectedSockets[i]->sendData(buf, size) < 0) {
					// Failed to send to this socket
					ConnectStatus st(false, (int)_connectedSockets.size()-1);
					_connectedSockets[i]->getRemote(st.addr, st.port);
					// Remove it from our list
					std::vector<TCPConnection*>::iterator it = _connectedSockets.begin();
					it += i;
					delete _connectedSockets[i];
					// Connected status callback with deleted address::port and total sockets connected
					_client->onDisconnect(_connectedSockets[i], st);
					_connectedSockets.erase(it);
				}
			}
			return true;
		}

		int numConnected()
		{
			SafeLock lck(&_mtx);
			return (int)_connectedSockets.size();
		}

	protected:
		// Called from derived classes
		void AddSocket(TCPConnection* sock)
		{
			SafeLock lck(&_mtx);
			sock->setProtocol(_protocol->create());
			_connectedSockets.push_back(sock);
			// Connected status callback with new address::port and total sockets connected
			ConnectStatus st(true, (int)_connectedSockets.size());
			sock->getRemote(st.addr, st.port);
			_client->onConnect(sock, st);
		}

		// Called from TCPConnection()
		void onEvent(TCPConnection* conn, BytesSizePair arg) override
		{
			_client->onEvent(conn, arg);
		}

		// Called from TCPConnection()
		void onReceiveError(TCPConnection* conn) override
		{
			// The protocol/connection can't receive any more. For a server connection
			// we only close the connection. The TCP Client need to connect again.
			conn->close();

			// Since we are called from within the connection we can't delete it here.
			// It will be deleted at next sendTo() call.
		}
		
		// All connected sockets
		std::vector<TCPConnection*> _connectedSockets;
		Lockable _mtx;
	
	private:
		TCPServerCallbacks* _client;
		TCPProtocol* _protocol;
	};
}
