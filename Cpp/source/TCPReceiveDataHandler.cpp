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

#include "OPSTypeDefs.h"
#include "TCPReceiveDataHandler.h"
#include "OPSArchiverIn.h"
#include "BasicError.h" 
#include "Domain.h"
#include "Participant.h"
#include "ReceiverFactory.h"
#include "CommException.h"
#include "TimeHelper.h"

namespace ops
{
	///TODO
	/// We want to send:
	///   "Info about which subscribers"
	///      @ connect
	///      sub added/removed
	/// We receive:
	///   "OPS Messages"
	///      Normal handling
	///   "Info about which publishers"
	///      Store info in map so that subscribers can ask if any publisher
	///      @ disconnect, clear publishers

    TCPReceiveDataHandler::TCPReceiveDataHandler(Topic top, Participant& part) :
		ReceiveDataHandler(top, part, 
			Receiver::createTCPClient(
				this, top.getDomainAddress(), top.getPort(), 
				part.getIOService(), top.getInSocketBufferSize())),
		_protocol(nullptr), _connected(false)
    {
	}

    TCPReceiveDataHandler::~TCPReceiveDataHandler()
    {
    }

	// Tell derived classes which topics that are active
	void TCPReceiveDataHandler::topicUsage(Topic& top, bool used)
	{
		///TODO Keep a list of all used topics, with count
		/// If a new topic is added or an old one deleted, send updates if connected
	}

    ///Override from Listener
    ///Called whenever the receiver has new data.
    void TCPReceiveDataHandler::onNewEvent(Notifier<BytesSizePair>* sender, BytesSizePair byteSizePair)
    {
		/// Here we got some data in our buffer
		///TODO Check if it is an internal TCP Transport info and if so handle it and start a new asynch read

		/// Otherwise let parent handle it (normal OPS trafic)
		ReceiveDataHandler::onNewEvent(sender, byteSizePair);
	}

	// Called from client when a connection is made
	void TCPReceiveDataHandler::onConnect(TCPConnection& conn, ConnectStatus status)
	{
		OPS_TCP_TRACE("RDH: onConnect()\n");
		// If first time, set the protocol to use
		if (!_protocol) {
			_protocol = new TCPOpsProtocol(TimeHelper::currentTimeMillis);
			conn.setProtocol(_protocol);
		}
		
		// Trig sending of "ops protocol probe" to see if server know new features.
		_protocol->sendProbe();

		_connected = true;
		ReceiveDataHandler::onNewEvent(nullptr, status);
	}

	// Called from client when a connection is closed
	// Ev. buffer used in asynchRead() is no longer in use
	void TCPReceiveDataHandler::onDisconnect(TCPConnection&, ConnectStatus status)
	{
		OPS_TCP_TRACE("RDH: onDisconnect()\n");
		_connected = false;
		// Need to reset protocol state in case the next connection is to a server with another version
		_protocol->resetProtocol();
		ReceiveDataHandler::onNewEvent(nullptr, status);
	}
}
