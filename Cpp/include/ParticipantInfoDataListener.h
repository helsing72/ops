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
#ifndef ops_ParticipantInfoDataListener_h
#define	ops_ParticipantInfoDataListener_h

#include "DataNotifier.h"
#include "ParticipantInfoData.h"
#include "Subscriber.h"
#include "SendDataHandler.h"
#include "Lockable.h"
#include "OPSExport.h"

namespace ops
{
	class Participant;

	class OPS_EXPORT ParticipantInfoDataListener : public DataListener
	{
	public:
		ParticipantInfoDataListener(Participant* part);

		void prepareForDelete();
		virtual ~ParticipantInfoDataListener();

		void onNewData(DataNotifier* notifier);

		void connectUdp(Topic& top, SendDataHandler* handler);
		void disconnectUdp(Topic& top, SendDataHandler* handler);

		void connectTcp(Topic& top, void* handler);
		void disconnectTcp(Topic& top, void* handler);

	private:
		Participant* participant;

		Lockable mutex;
		Subscriber* partInfoSub;
		SendDataHandler* sendDataHandler;

		int numUdpTopics;

		bool setupSubscriber();
		void removeSubscriber();
		
	};


}

#endif
