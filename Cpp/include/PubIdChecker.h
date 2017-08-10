#pragma once

/**
*
* Copyright (C) 2017 Lennart Andersson.
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

#include <map>
#include "OPSMessage.h"
#include "Notifier.h"

namespace ops {

	typedef struct {
		OPSMessage* mess;		// Pointer to received message
		__int64 expectedPubID;
		bool newPublisher;		// True for a new Publisher, False for a detected Sequence Error
	} PublicationIdNotification_T;

	class PublicationIdChecker : public Notifier<PublicationIdNotification_T>
	{
	private:
		typedef struct
		{
			Address_T Addr;
			int Port;
			__int64 expectedPubID;
		} Entry_T;

		std::map<InternalKey_T, Entry_T> _map;
		Entry_T* _prev;

	public:
		PublicationIdChecker() : _prev(nullptr) {}

		// Note: The Check() won't work if an application uses several publishers to publish on the same topic, since
		// the PublicationID is unique for each publisher, but the underlaying transport can use the same IP & Port,
		// eg. UDP point-to-point only use ONE port for the application for sending all messages, other transports
		// uses one sender per "transport::adress::port".
		virtual void Check(OPSMessage* message)
		{
			// Get sender info
			Address_T addr;
			int port;
			message->getSource(addr, port);

			if ((_prev == nullptr) || (_prev->Port != port) || (_prev->Addr != addr)) {
				// First or Another sender
				// Make key
				InternalKey_T key = addr;
				key += "::";
				key += NumberToString(port); 
				
				// Look up entry
				auto result = _map.find(key);
				if (result != _map.end()) {
					// found
					_prev = &_map[key];
				} else {
					// not found, create a new one
					_prev = &_map[key];
					_prev->expectedPubID = message->getPublicationID();
					// Notify listeners, that a new publisher arrived
					PublicationIdNotification_T arg;
					arg.expectedPubID = _prev->expectedPubID;
					arg.mess = message;
					arg.newPublisher = true;
					notifyNewEvent(arg);
				}
			}

			// _prev points to correct entry
			if (_prev->expectedPubID != message->getPublicationID()) {
				// Notify listeners, that sequence didn't match
				PublicationIdNotification_T arg;
				arg.expectedPubID = _prev->expectedPubID;
				arg.mess = message;
				arg.newPublisher = false;
				notifyNewEvent(arg);
			}

			// Update for next message
			_prev->expectedPubID = message->getPublicationID() + 1;
		}
	};
}
