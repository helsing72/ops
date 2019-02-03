/**
* 
* Copyright (C) 2006-2009 Anton Gravestam.
* Copyright (C) 2018-2019 Lennart Andersson.
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

#include "OPSExport.h"
#include "Notifier.h"
#include "BytesSizePair.h"
#include "ReceiveDataHandler.h"
#include "TCPClientBase.h"
#include "TCPOpsProtocol.h"

namespace ops
{
	class OPS_EXPORT TCPReceiveDataHandler : public ReceiveDataHandler, TCPClientCallbacks
	{
	public:
		TCPReceiveDataHandler(Topic top, Participant* part);
		virtual ~TCPReceiveDataHandler();

	protected:
		void onNewEvent(Notifier<BytesSizePair>* sender, BytesSizePair byteSizePair) override;

		// Called from client when a connection is made
		void onConnect(TCPConnection& conn, ConnectStatus status) override;

		// Called from client when a connection is closed
		// Ev. buffer used in asynchRead() is no longer in use
		void onDisconnect(TCPConnection& conn, ConnectStatus status) override;

		// Tell derived classes which topics that are active
		void topicUsage(Topic& top, bool used) override;

	private:
		TCPOpsProtocol* _protocol;
		bool _connected;
	};
	
}
