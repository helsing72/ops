#include "OPSTypeDefs.h"
#include "ParticipantInfoDataListener.h"
#include "McUdpSendDataHandler.h"
#include "Participant.h"
#include "BasicError.h"

namespace ops
{

	ParticipantInfoDataListener::ParticipantInfoDataListener(Participant* part):
		participant(part),
		partInfoSub(NULL),
		sendDataHandler(NULL),
		numUdpTopics(0)
    {
    }

	///Called when a new message is received. Running on the boost thread.
    void ParticipantInfoDataListener::onNewData(DataNotifier* notifier)
    {
        Subscriber* sub = dynamic_cast<Subscriber*> (notifier);
        if (sub)
        {
            ParticipantInfoData* partInfo = dynamic_cast<ParticipantInfoData*> (sub->getMessage()->getData());
            if (partInfo)
            {
				// Is it on our domain?
				if (partInfo->domain == participant->domainID) {
					SafeLock lock(&mutex);
					if (sendDataHandler != NULL) {
		                for (unsigned int i = 0; i < partInfo->subscribeTopics.size(); i++)
			            {
							if ( (partInfo->subscribeTopics[i].transport == Topic::TRANSPORT_UDP) &&
								 participant->hasPublisherOn(partInfo->subscribeTopics[i].name) ) {
					            //Do an add sink here
							    dynamic_cast<McUdpSendDataHandler*>(sendDataHandler)->addSink(partInfo->subscribeTopics[i].name, partInfo->ip, partInfo->mc_udp_port);
							}
						}
					}
	                for (unsigned int i = 0; i < partInfo->publishTopics.size(); i++)
		            {
						if (partInfo->publishTopics[i].transport == Topic::TRANSPORT_TCP) {
							///TODO lookup topic in map. If found call handler
						}
					}
				}
            }
            else
            {
				BasicError err("ParticipantInfoDataListener", "onNewData", "Data could not be cast as expected.");
                participant->reportError(&err);
            }
        }
        else
        {
			BasicError err("ParticipantInfoDataListener", "onNewData", "Subscriber could not be cast as expected.");
            participant->reportError(&err);
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
		if (participant->getDomain()->getMetaDataMcPort() == 0) {
			return false;
		}

		partInfoSub = new Subscriber(participant->createParticipantInfoTopic());
		partInfoSub->addDataListener(this);
		partInfoSub->start();

		return true;
	}

	void ParticipantInfoDataListener::removeSubscriber()
	{
		if (partInfoSub) delete partInfoSub;
		partInfoSub = NULL;
	}

	void ParticipantInfoDataListener::connectUdp(Topic& top, SendDataHandler* handler)
	{
		SafeLock lock(&mutex);
		if (!partInfoSub) {
			if (!setupSubscriber()) {
				// Generate an error message if we come here with domain->getMetaDataMcPort() == 0,
				// it means that we have UDP topics that require meta data but user has disabled it.
				ErrorMessage_T msg("UDP topic '");
				msg += top.getName();
				msg += "' won't work since Meta Data disabled in config-file";
				BasicError err("ParticipantInfoDataListener", "connectUdp", msg);
				participant->reportError(&err);
				return;
			}
		}

		// Since we only have one common UDP SendDataHandler, its enough to count connected topics
		numUdpTopics++;

		sendDataHandler = handler;
	}

	void ParticipantInfoDataListener::disconnectUdp(Topic& top, SendDataHandler* handler)
	{
		UNUSED(top);
		UNUSED(handler);
		SafeLock lock(&mutex);

		// Remove topic from list so we know if the subscriber is needed
		numUdpTopics--;

		if (numUdpTopics == 0) {
			sendDataHandler = NULL;

///TODO			if (num tcp topics == 0) {
				removeSubscriber();
///			}
		}
	}

	void ParticipantInfoDataListener::connectTcp(Topic& top, void* handler)
	{
		UNUSED(handler);
		SafeLock lock(&mutex);
		if (!partInfoSub) {
			if (!setupSubscriber()) {
				// Generate an error message if we come here with domain->getMetaDataMcPort() == 0,
				// it means that we have TCP topics that require meta data but user has disabled it.
				ErrorMessage_T msg("TCP topic '");
				msg += top.getName();
				msg += "' won't work since Meta Data disabled in config-file";
				BasicError err("ParticipantInfoDataListener", "connectTcp", msg);
				participant->reportError(&err);
				return;
			}
		}
		///TODO add to map
	}

	void ParticipantInfoDataListener::disconnectTcp(Topic& top, void* handler)
	{
		UNUSED(top);
		UNUSED(handler);
		SafeLock lock(&mutex);
		///TODO remove from map
	}

}
