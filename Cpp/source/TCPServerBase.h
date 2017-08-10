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
 * File:   TCPServerBase.h
 * Author: Anton Gravestam
 *
 * Created on den 22 oktober 2008, 20:01
 */

#pragma once

#include <iostream>
#include <mutex>

#include "Sender.h"

#include "Participant.h"
#include "BasicError.h"

namespace ops
{

	class TCPServerBase : public Sender
    {
    public:
		TCPServerBase() 
		{
		}
		
		virtual ~TCPServerBase()
		{
			close();
		}

		virtual void close()
		{
			std::lock_guard<std::mutex> lck(_mtx);
			for (int i = (int)_connectedSockets.size() - 1; i >= 0; i--) {
				delete _connectedSockets[i];
			}
			_connectedSockets.clear();
		}

		///Sends buf to any Receiver connected to this Sender, ip and port are discarded and can be left blank.
        virtual bool sendTo(char* buf, int size, const Address_T& ip, int port)
		{
			UNUSED(ip);
			UNUSED(port);
			std::lock_guard<std::mutex> lck(_mtx);
			// Send to anyone connected. Loop backwards to avoid problems when removing broken sockets
			for (int i = (int)_connectedSockets.size() - 1; i >= 0 ; i--) {
				// Send the data
				if (_connectedSockets[i]->send(buf, size) < 0) {
					// Failed to send to this socket. Remove it from our list
					std::vector<SocketSender*>::iterator it;
					it = _connectedSockets.begin();
					it += i;
					delete _connectedSockets[i];
					_connectedSockets.erase(it);
				}
			}
			return true;
		}

	protected:
		//Internal helper class for each connected socket
		class SocketSender
		{
		public:
			virtual ~SocketSender() {}
			virtual int send(char* buf, int size) = 0;
		};

		int numConnected()
		{
			return (int)_connectedSockets.size();
		}

		void AddSocket(SocketSender* sock)
		{
			std::lock_guard<std::mutex> lck(_mtx);
			_connectedSockets.push_back(sock);
		}

		// All connected sockets
		std::vector<SocketSender*> _connectedSockets;
		std::mutex _mtx;
    };
}

