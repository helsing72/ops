
#include "OPSTypeDefs.h"
#include "ReceiveDataHandlerFactory.h"
#include "ReceiveDataHandler.h"
#include "Participant.h"
#include "BasicError.h"
#include "NetworkSupport.h"

namespace ops
{
    ReceiveDataHandlerFactory::ReceiveDataHandlerFactory(Participant* participant)
    {
        UNUSED(participant);
    }

	InternalKey_T ReceiveDataHandlerFactory::makeKey(Topic& top, IOService* ioServ)
	{
		// Since topics can use the same port for transports multicast & tcp, or 
		// use transport udp which in most cases use a single ReceiveDataHandler, 
		// we need to return the same ReceiveDataHandler in these cases.
		// Make a key with the transport info that uniquely defines the receiver.
		InternalKey_T key(top.getTransport());
		if (top.getTransport() == Topic::TRANSPORT_UDP) {
			if (!isMyNodeAddress(top.getDomainAddress(), ioServ)) {
				return key;
			}
		} 
		key += "::";
		key += top.getDomainAddress();
		key += "::";
		key += NumberToString(top.getPort());
		return key;
	}

    ReceiveDataHandler* ReceiveDataHandlerFactory::getReceiveDataHandler(Topic& top, Participant* participant)
    {
		// Make a key with the transport info that uniquely defines the receiver.
		InternalKey_T key = makeKey(top, participant->getIOService());

        SafeLock lock(&garbageLock);
        if (receiveDataHandlerInstances.find(key) != receiveDataHandlerInstances.end())
        {
            // If we already have a ReceiveDataHandler for this topic, use it.
			ReceiveDataHandler* rdh = receiveDataHandlerInstances[key];

            // Check if any of the topics have a sample size larger than MAX_SEGMENT_SIZE
            // This will lead to a problem when using the same port or using UDP, if samples becomes > MAX_SEGMENT_SIZE
			if ((rdh->getSampleMaxSize() > OPSConstants::PACKET_MAX_SIZE) || (top.getSampleMaxSize() > OPSConstants::PACKET_MAX_SIZE))
            {
				ErrorMessage_T msg;
				if (top.getTransport() == Topic::TRANSPORT_UDP) {
					msg = "Warning: UDP Transport is used with Topics with 'sampleMaxSize' > ";
					msg += NumberToString(OPSConstants::PACKET_MAX_SIZE);
				} else {
					msg += "Warning: Same port (";
					msg += NumberToString(top.getPort());
					msg += ") is used with Topics with 'sampleMaxSize' > ";
					msg += NumberToString(OPSConstants::PACKET_MAX_SIZE);
				}
				BasicError err("ReceiveDataHandlerFactory", "getReceiveDataHandler", msg);
				participant->reportError(&err);
            }
            return rdh;
        }
        else if ( (top.getTransport() == Topic::TRANSPORT_MC) || (top.getTransport() == Topic::TRANSPORT_TCP) )
        {
            ReceiveDataHandler* newReceiveDataHandler = new ReceiveDataHandler(top, participant);
            receiveDataHandlerInstances[key] = newReceiveDataHandler;
            return newReceiveDataHandler;
        }
        else if (top.getTransport() == Topic::TRANSPORT_UDP)
        {
	        ReceiveDataHandler* udpReceiveDataHandler = new ReceiveDataHandler(top, participant);

			if (key == top.getTransport()) {
				Receiver* recv = udpReceiveDataHandler->getReceiver();
				participant->setUdpTransportInfo(recv->getLocalAddress(), recv->getLocalPort());
			}
            
			receiveDataHandlerInstances[key] = udpReceiveDataHandler;
            return udpReceiveDataHandler;
        }
        else //For now we can not handle more transports
        {
			//Signal an error by returning nullptr.
			ErrorMessage_T msg = "Unknown transport for Topic: ";
			msg += top.getName();
			BasicError err("ReceiveDataHandlerFactory", "getReceiveDataHandler", msg);
			participant->reportError(&err);
            return nullptr;
        }
    }

    void ReceiveDataHandlerFactory::releaseReceiveDataHandler(Topic& top, Participant* participant)
    {
		// Make a key with the transport info that uniquely defines the receiver.
		InternalKey_T key = makeKey(top, participant->getIOService());

		SafeLock lock(&garbageLock);
        if (receiveDataHandlerInstances.find(key) != receiveDataHandlerInstances.end())
        {
            ReceiveDataHandler* rdh = receiveDataHandlerInstances[key];
            if (rdh->getNrOfListeners() == 0)
            {
                //Time to mark this receiveDataHandler as garbage.
                receiveDataHandlerInstances.erase(receiveDataHandlerInstances.find(key));

                rdh->stop();

				if (key == Topic::TRANSPORT_UDP) {
					participant->setUdpTransportInfo("", 0);
				}

                garbageReceiveDataHandlers.push_back(rdh);
            }
        }
    }

    void ReceiveDataHandlerFactory::cleanUpReceiveDataHandlers()
    {
        SafeLock lock(&garbageLock);
        
        for (int i = (int)garbageReceiveDataHandlers.size() - 1; i >= 0; i--)
        {
            if (garbageReceiveDataHandlers[i]->numReservedMessages() == 0)
            {
                delete garbageReceiveDataHandlers[i];
                std::vector<ReceiveDataHandler*>::iterator iter = garbageReceiveDataHandlers.begin() + i;
                garbageReceiveDataHandlers.erase(iter);
            }
        }
    }

	bool ReceiveDataHandlerFactory::cleanUpDone()
	{
        SafeLock lock(&garbageLock);
		return garbageReceiveDataHandlers.size() == 0;
	}

    ReceiveDataHandlerFactory::~ReceiveDataHandlerFactory()
    {
    }

}
