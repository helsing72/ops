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

#include "SendDataHandler.h"
#include "Lockable.h"
#include "Sender.h"
#include "IOService.h"
#include "TCPServerBase.h"

namespace ops
{
    class TCPSendDataHandler : public SendDataHandler, TCPServerCallbacks
    {
    public:
        TCPSendDataHandler(IOService* ioService, Topic& topic)
        {
			sender = Sender::createTCPServer(this, ioService, topic.getDomainAddress(), topic.getPort(), topic.getOutSocketBufferSize());
        }

        bool sendData(char* buf, int bufSize, Topic& topic)
        {
            UNUSED(topic);
            SafeLock lock(&mutex);
            //We dont "sendTo" but rather lets the server (sender) send to all connected clients.
            bool result = sender->sendTo(buf, bufSize, "", 0);
            return result;
        }

		// Called from server when a new connection is accepted
		// Could be used to call conn->asynchRead(buffer, size)
		virtual void onConnect(TCPConnection* conn, ConnectStatus status)
		{
			// Notify parent
			//onNewEvent(nullptr, status);
		}

		// Called from server when data has been filled into given buffer
		// A new call to conn->asynchRead(buffer, size) need to be done to continue to read
		virtual void onEvent(TCPConnection* conn, BytesSizePair arg)

		{

		}



		// Called from server when a connection has been deleted
		// NOTE: 'conn' is invalid and is only provided as an ID.
		// Ev. buffer used in asynchRead() is no longer in use
		virtual void onDisconnect(TCPConnection* conn, ConnectStatus status)
		{
			// Notify parent
			//onNewEvent(nullptr, status);
		}

        virtual ~TCPSendDataHandler()
        {
            SafeLock lock(&mutex);
			delete sender;
        }
    };

}
