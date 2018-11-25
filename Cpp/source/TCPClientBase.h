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

#include "Receiver.h"
#include "BytesSizePair.h"
#include "Participant.h"
#include "BasicError.h"
#include "TCPProtocol.h"
#include "ConnectStatus.h"
#include "TCPConnection.h"

namespace ops
{

    class TCPClientBase : public Receiver, public Notifier<ConnectStatus>, protected TCPConnectionCallbacks
    {
    public:
        TCPClientBase(TCPProtocol* protocol, TCPConnection* connection) :
			_connection(connection), _cs(false, 0)
        {
			_connection->setProtocol(protocol);
        }

		virtual ~TCPClientBase()
		{
			// _connection should be deleted by derived class(es)
		}

        void asynchWait(char* bytes, int size) override
        {
			_connection->asynchWait(bytes, size);
        }

		int sendTo(char* buf, int size)
		{
			return _connection->sendData(buf, size);
		}

		virtual bool isConnected() = 0;

	protected:
		TCPConnection* _connection;

		// Should be called by the derived classes
		void connected(bool value)
		{
			bool doNotify = _cs.connected != value;
			_cs.connected = value;
			_cs.totalNo = value ? 1 : 0;
			if (value) {
				getSource(_cs.addr, _cs.port);
				Notifier<BytesSizePair>::notifyNewEvent(BytesSizePair(nullptr, -5)); //Connection was down but has been reastablished.
			}
			if (doNotify) Notifier<ConnectStatus>::notifyNewEvent(_cs);
		}

		// Called from TCPConnection()
		void onEvent(TCPConnection* prot, BytesSizePair arg) override
		{
			UNUSED(prot);
			Notifier<BytesSizePair>::notifyNewEvent(arg);
		}

		// Called from TCPConnection()
		void onReceiveError(TCPConnection* prot) override
		{
			UNUSED(prot);
			//Report error
			ops::BasicError err("TCPClient", "handle_received_data", "Error in receive.");
			Participant::reportStaticError(&err);

			//Close the socket
			stop();

			//Notify our user
			Notifier<BytesSizePair>::notifyNewEvent(BytesSizePair(nullptr, -1));

			//Try to connect again
			start();
		}

	private:
		ConnectStatus _cs;
	};
}
