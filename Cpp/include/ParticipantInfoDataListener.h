/**
* 
* Copyright (C) 2006-2009 Anton Gravestam.
* Copyright (C) 2019 Lennart Andersson.
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

#include <map>

#include "DataNotifier.h"
#include "ParticipantInfoData.h"
#include "Subscriber.h"
#include "SendDataHandler.h"
#include "ReceiveDataHandler.h"
#include "Lockable.h"
#include "OPSExport.h"

namespace ops
{
	class Participant;

	class OPS_EXPORT ParticipantInfoDataListener : public DataListener
	{
	public:
		ParticipantInfoDataListener(Participant& part);

		void prepareForDelete();
		virtual ~ParticipantInfoDataListener();

		virtual void onNewData(DataNotifier* notifier) override;

		void connectUdp(Topic& top, SendDataHandler* handler);
		void disconnectUdp(Topic& top, SendDataHandler* handler);

		void connectTcp(ObjectName_T& top, std::shared_ptr<ReceiveDataHandler> handler);
		void disconnectTcp(ObjectName_T& top, std::shared_ptr<ReceiveDataHandler> handler);

	private:
		Participant& participant;

		Lockable mutex;
		Subscriber* partInfoSub;
		SendDataHandler* sendDataHandler;

		std::map<ObjectName_T, std::shared_ptr<ReceiveDataHandler>> rcvDataHandlers;

		int numUdpTopics;

		bool setupSubscriber();
		void removeSubscriber();
	};
}
#endif
