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
#include "OPSTypeDefs.h"
#include "ParticipantInfoDataListener.h"
#include "McUdpSendDataHandler.h"
#include "TCPReceiveDataHandler.h"
#include "Participant.h"
#include "BasicError.h"
#include "NetworkSupport.h"

namespace ops
{

	ParticipantInfoDataListener::ParticipantInfoDataListener(Participant& part):
		participant(part),
		partInfoSub(nullptr),
		sendDataHandler(nullptr),
		numUdpTopics(0)
    {
    }

	///Called when a new message is received. Running on the boost thread.
    void ParticipantInfoDataListener::onNewData(DataNotifier* const notifier)
    {
        Subscriber* sub = dynamic_cast<Subscriber*> (notifier);
        if (sub != nullptr) {
            ParticipantInfoData* partInfo = dynamic_cast<ParticipantInfoData*> (sub->getMessage()->getData());
            if (partInfo != nullptr) {
				// Is it on our domain?
				if (partInfo->domain == participant.domainID) {
					SafeLock lock(&mutex);
					if (sendDataHandler != nullptr) {
						if (partInfo->mc_udp_port != 0) {
							for (auto x : partInfo->subscribeTopics) {
								if ((x.transport == Topic::TRANSPORT_UDP) && participant.hasPublisherOn(x.name)) {
									//Do an add sink here
									dynamic_cast<McUdpSendDataHandler*>(sendDataHandler)->addSink(x.name, partInfo->ip, partInfo->mc_udp_port);
								}
							}
						}
					}
					for (auto& x : partInfo->publishTopics) {
						if ((x.transport == Topic::TRANSPORT_TCP) && participant.hasSubscriberOn(x.name)) {
							// Lookup topic in map. If found call handler
							auto result = rcvDataHandlers.find(x.name);
							if (result != rcvDataHandlers.end()) {
								dynamic_cast<TCPReceiveDataHandler*>(result->second)->AddReceiveChannel(x.name, x.address, x.port);
							}
						}
					}
				}
            }
            else
            {
				BasicError err("ParticipantInfoDataListener", "onNewData", "Data could not be cast as expected.");
                participant.reportError(&err);
            }
        }
        else
        {
			BasicError err("ParticipantInfoDataListener", "onNewData", "Subscriber could not be cast as expected.");
            participant.reportError(&err);
        }
    }

    ParticipantInfoDataListener::~ParticipantInfoDataListener()
    {
	}

	void ParticipantInfoDataListener::prepareForDelete()
	{
		SafeLock lock(&mutex);
		// We can't remove the Subscriber in the destructor, since the delete of the Subscriber
		// requires objects that already has been deleted when the participant delete us
		// (for the case when user has subscribers left when deleting the participant. 
		// Normally we have matching calls to connect & disconnect, so subscriber is already deleted)
		removeSubscriber();
	}

	bool ParticipantInfoDataListener::setupSubscriber()
	{
		// Check that user hasn't disabled the meta data
		if (participant.getDomain()->getMetaDataMcPort() == 0) {
			return false;
		}

		partInfoSub = new Subscriber(participant.createParticipantInfoTopic());
		partInfoSub->addDataListener(this);
		partInfoSub->start();

		return true;
	}

	void ParticipantInfoDataListener::removeSubscriber()
	{
		if (partInfoSub != nullptr) { delete partInfoSub; }
		partInfoSub = nullptr;
	}

	void ParticipantInfoDataListener::connectUdp(Topic& top, SendDataHandler* const handler)
	{
		SafeLock lock(&mutex);
		if (partInfoSub == nullptr) {
			if (!setupSubscriber()) {
				if (!isValidNodeAddress(top.getDomainAddress())) {
					// Generate an error message if we come here with domain->getMetaDataMcPort() == 0,
					// it means that we have UDP topics that require meta data but user has disabled it.
					ErrorMessage_T msg("UDP topic '");
					msg += top.getName();
					msg += "' won't work since Meta Data disabled in config-file";
					BasicError err("ParticipantInfoDataListener", "connectUdp", msg);
					participant.reportError(&err);
				}
			}
		}

		// Since we only have one common UDP SendDataHandler, its enough to count connected topics
		numUdpTopics++;

		sendDataHandler = handler;
	}

	void ParticipantInfoDataListener::disconnectUdp(Topic& top, SendDataHandler* const handler)
	{
		UNUSED(top);
		UNUSED(handler);
		SafeLock lock(&mutex);

		// Remove topic from list so we know if the subscriber is needed
		numUdpTopics--;

		if (numUdpTopics == 0) {
			sendDataHandler = nullptr;

			if (rcvDataHandlers.size() == 0) {
				removeSubscriber();
			}
		}
	}

	void ParticipantInfoDataListener::connectTcp(ObjectName_T& top, ReceiveDataHandler* const handler)
	{
		SafeLock lock(&mutex);
		if (partInfoSub == nullptr) {
			if (!setupSubscriber()) {
				// Generate an error message if we come here with domain->getMetaDataMcPort() == 0,
				// it means that we have TCP topics that require meta data but user has disabled it.
				ErrorMessage_T msg("TCP topic '");
				msg += top;
				msg += "' won't work since Meta Data disabled in config-file";
				BasicError err("ParticipantInfoDataListener", "connectTcp", msg);
				participant.reportError(&err);
				return;
			}
		}
		
		// Add to map if not already there
		if (rcvDataHandlers.find(top) != rcvDataHandlers.end()) {
			ReceiveDataHandler* rdh = rcvDataHandlers[top];
			if (rdh != handler) {
				ErrorMessage_T msg("TCP topic '");
				msg += top;
				msg += "' already registered for another RDH";
				BasicError err("ParticipantInfoDataListener", "connectTcp", msg);
				participant.reportError(&err);
				return;
			}
		} else {
			rcvDataHandlers[top] = handler;
		}
	}

	void ParticipantInfoDataListener::disconnectTcp(ObjectName_T& top, ReceiveDataHandler* const handler)
	{
		SafeLock lock(&mutex);

		// Remove from map
		auto result = rcvDataHandlers.find(top);
		if (result != rcvDataHandlers.end()) {
			ReceiveDataHandler* rdh = rcvDataHandlers[top];
			if (rdh != handler) {
				ErrorMessage_T msg("TCP topic '");
				msg += top;
				msg += "' atempt to remove topic for another RDH";
				BasicError err("ParticipantInfoDataListener", "connectTcp", msg);
				participant.reportError(&err);
				return;
			}
			rcvDataHandlers.erase(result);

			if (rcvDataHandlers.size() == 0) {
				if (numUdpTopics == 0) {
					removeSubscriber();
				}
			}
		}
	}

}
