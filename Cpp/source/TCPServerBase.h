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

#include "OPSTypeDefs.h"

#include "Notifier.h"
#include "Lockable.h"
#include "Sender.h"
#include "ConnectStatus.h"
#include "TCPProtocol.h"

namespace ops
{
	class TCPServerBase : public Sender, public Notifier<ConnectStatus>
    {
    public:
		TCPServerBase(TCPProtocol* protocol) :
			_protocol(protocol)
		{}

		virtual ~TCPServerBase()
		{
			close();
			delete _protocol;
		}

		virtual void close()
		{
			SafeLock lck(&_mtx);
			for (int i = (int)_connectedSockets.size() - 1; i >= 0; i--) {
				delete _connectedSockets[i];
			}
			_connectedSockets.clear();
		}

		///Sends buf to all Receivers connected to this Sender, ip and port are discarded and can be left blank.
        virtual bool sendTo(char* buf, int size, const Address_T& ip, int port)
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
					std::vector<SocketSender*>::iterator it = _connectedSockets.begin();
					it += i;
					delete _connectedSockets[i];
					_connectedSockets.erase(it);
					// Connected status callback with deleted address::port and total sockets connected
					notifyNewEvent(st);
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
		//Internal helper class for each connected socket
		class SocketSender : TCPProtocolUser
		{
		public:
			virtual ~SocketSender()
			{
				if (_protocol) delete _protocol;
			}
			
			virtual void getRemote(Address_T& address, int&port) = 0;

			int sendData(char* buf, int size)
			{
				return _protocol->sendData(buf, size);
			}
			
		protected:
			virtual int send(char* buf, int size) = 0;

		private:
			friend class TCPServerBase;

			TCPProtocol* _protocol = nullptr;

			void setProtocol(TCPProtocol* prot)
			{
				_protocol = prot;
				_protocol->connect(this);
			}

			// Needed by protocol
			bool isConnected(TCPProtocol* prot) override
			{
				UNUSED(prot);
				return true;	// Currently not used for TCPServer
			}

			// Needed by protocol
			void startAsyncRead(TCPProtocol* prot, char* bytes, int size) override
			{
				UNUSED(prot);
				UNUSED(bytes);
				UNUSED(size);	// Currently not used for TCPServer
			}

			// Needed by protocol
			void onEvent(TCPProtocol* prot, BytesSizePair arg) override
			{
				UNUSED(prot);	// Currently not used for TCPServer
			}

			// Called from protocol
			virtual int sendBuffer(TCPProtocol* prot, char* bytes, int size) override
			{
				return send(bytes, size);
			}
		};

		void AddSocket(SocketSender* sock)
		{
			SafeLock lck(&_mtx);
			sock->setProtocol(_protocol->create());
			_connectedSockets.push_back(sock);
			// Connected status callback with new address::port and total sockets connected
			ConnectStatus st(true, (int)_connectedSockets.size());
			sock->getRemote(st.addr, st.port);
			notifyNewEvent(st);
		}

		// All connected sockets
		std::vector<SocketSender*> _connectedSockets;
		Lockable _mtx;
	
	private:
		TCPProtocol* _protocol;
	};
}
