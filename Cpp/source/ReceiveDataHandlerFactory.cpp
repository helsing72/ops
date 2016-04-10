#include <sstream>

#include "OPSTypeDefs.h"
#include "ReceiveDataHandlerFactory.h"

#include "ReceiveDataHandler.h"
#include "UDPReceiver.h"
#include "BasicError.h"


namespace ops
{

    ReceiveDataHandlerFactory::ReceiveDataHandlerFactory(Participant* participant)
    {
    }

	std::string ReceiveDataHandlerFactory::makeKey(Topic& top)
	{
		// Since topics can use the same port for transports multicast & tcp, or 
		// use transport udp which always use a single ReceiveDataHandler, 
		// we need to return the same ReceiveDataHandler in these cases.
		// Make a key with the transport info that uniquely defines the receiver.
		if (top.getTransport() == Topic::TRANSPORT_UDP) {
			return top.getTransport();
		} else {
			std::ostringstream myPort;
			myPort << top.getPort() << std::ends;
			return top.getTransport() + "::" + top.getDomainAddress() + "::" + myPort.str();
		}
	}

    ReceiveDataHandler* ReceiveDataHandlerFactory::getReceiveDataHandler(Topic& top, Participant* participant)
    {
		// Make a key with the transport info that uniquely defines the receiver.
		std::string key = makeKey(top);

        SafeLock lock(&garbageLock);
        if (receiveDataHandlerInstances.find(key) != receiveDataHandlerInstances.end())
        {
            // If we already have a ReceiveDataHandler for this topic, use it.
			ReceiveDataHandler* rdh = receiveDataHandlerInstances[key];

            // Check if any of the topics have a sample size larger than MAX_SEGMENT_SIZE
            // This will lead to a problem when using the same port or using UDP, if samples becomes > MAX_SEGMENT_SIZE
			if ((rdh->getSampleMaxSize() > OPSConstants::PACKET_MAX_SIZE) || (top.getSampleMaxSize() > OPSConstants::PACKET_MAX_SIZE))
            {
				std::ostringstream myMessage;
				if (top.getTransport() == Topic::TRANSPORT_UDP) {
					myMessage <<
						"Warning: UDP Transport is used with Topics with 'sampleMaxSize' > " << OPSConstants::PACKET_MAX_SIZE << std::ends;
				} else {
					myMessage <<
						"Warning: Same port (" << top.getPort() << 
						") is used with Topics with 'sampleMaxSize' > " << OPSConstants::PACKET_MAX_SIZE << std::ends;
				}
				BasicError err("ReceiveDataHandlerFactory", "getReceiveDataHandler", myMessage.str());
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

			participant->setUdpTransportInfo(
				((UDPReceiver*) udpReceiveDataHandler->getReceiver())->getAddress(),
				((UDPReceiver*) udpReceiveDataHandler->getReceiver())->getPort() );
            
			receiveDataHandlerInstances[key] = udpReceiveDataHandler;
            return udpReceiveDataHandler;
        }
        else //For now we can not handle more transports
        {
            //Signal an error by returning NULL.
			BasicError err("ReceiveDataHandlerFactory", "getReceiveDataHandler", "Unknown transport for Topic: " + top.getName());
			participant->reportError(&err);
            return NULL;
        }
    }

    void ReceiveDataHandlerFactory::releaseReceiveDataHandler(Topic& top, Participant* participant)
    {
		// Make a key with the transport info that uniquely defines the receiver.
		std::string key = makeKey(top);

		SafeLock lock(&garbageLock);
        if (receiveDataHandlerInstances.find(key) != receiveDataHandlerInstances.end())
        {
            ReceiveDataHandler* rdh = receiveDataHandlerInstances[key];
            if (rdh->getNrOfListeners() == 0)
            {
                //Time to mark this receiveDataHandler as garbage.
                receiveDataHandlerInstances.erase(receiveDataHandlerInstances.find(key));

                rdh->stop();

				if (top.getTransport() == Topic::TRANSPORT_UDP) {
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
