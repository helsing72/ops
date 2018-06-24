#pragma once

/**
*
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

#include "OPSTypeDefs.h"

#ifdef OPS_ENABLE_DEBUG_HANDLER

#include "opsidls/DebugRequestResponseData.h"

namespace ops {

	// Forward Declaration
	class Participant;
	class Publisher;
	class Subscriber;

	// Interface for Debug callback
	class DebugNotifyInterface
	{
	public:
		virtual void onRequest(opsidls::DebugRequestResponseData& req, opsidls::DebugRequestResponseData& resp) = 0;
	};

	class DebugHandler
	{
		friend class Participant;
		friend class Publisher;
		friend class Subscriber;
	public:
		DebugHandler(Participant* part);
		~DebugHandler();

		// Key used as filter when listening on the debug topic for DebugRequestResponseData messages.
		// Should be set by application to a unique key in the system
		static void SetKey(const ObjectKey_T& key);

		// Used by application to set a handler for "Generic Command" (50)
		void SetAppCallback(DebugNotifyInterface* client);

	protected:
		void Start();

		// Register/Unregister with the debug handler
		void RegisterPub(DebugNotifyInterface* client, ObjectName_T topicName);
		void UnregisterPub(DebugNotifyInterface* client, ObjectName_T topicName);

		void RegisterSub(DebugNotifyInterface* client, ObjectName_T topicName);
		void UnregisterSub(DebugNotifyInterface* client, ObjectName_T topicName);

	private:
		class InternalDebugListener;
		InternalDebugListener* _pimpl;
	};
}
#endif
