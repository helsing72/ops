
#include <sstream>

#include "OPSTypeDefs.h"
#include "SendDataHandlerFactory.h"
#include "SendDataHandler.h"
#include "Participant.h"
#include "McSendDataHandler.h"
#include "McUdpSendDataHandler.h"
#include "TCPSendDataHandler.h"
#include "Domain.h"

namespace ops
{

	SendDataHandlerFactory::SendDataHandlerFactory(Participant* part):
		participant(part)
	{	
		// There is only one McUdpSendDataHandler for each participant
		udpSendDataHandler = NULL;
	}

	SendDataHandlerFactory::~SendDataHandlerFactory()
	{
		///TODO cleanup
	}
	
	SendDataHandler* SendDataHandlerFactory::getSendDataHandler(Topic& top, Participant* participant)
	{
		// We need to store SendDataHandlers with more than just the name as key.
		// Since topics can use the same port, we need to return the same SendDataHandler.
		// Make a key with the transport info that uniquely defines the receiver.
		std::ostringstream myStream;
		myStream << top.getPort() << std::ends;
		std::string key = top.getTransport() + "::" + top.getDomainAddress() + "::" + myStream.str();

		SafeLock lock(&mutex);

		if(sendDataHandlers.find(key) != sendDataHandlers.end())
		{
			return sendDataHandlers[key];
		}			

		std::string localIf = Domain::doSubnetTranslation(top.getLocalInterface(), participant->getIOService());

		int ttl = top.getTimeToLive();

		if(top.getTransport() == Topic::TRANSPORT_MC)
		{
			SendDataHandler* newSendDataHandler = new McSendDataHandler(top, localIf, ttl); 
			sendDataHandlers[key] = newSendDataHandler;
			return newSendDataHandler;
		}
		else if(top.getTransport() == Topic::TRANSPORT_UDP)
		{
			if(udpSendDataHandler == NULL)
			{
				// We have only one sender for all topics, so use the domain value for buffer size
				udpSendDataHandler = new McUdpSendDataHandler(localIf, 
															  ttl,
															  participant->getDomain()->getOutSocketBufferSize()); 
			}

			// Setup a listener on the participant info data published by participants on our domain.
			// We use the information for topics with UDP as transport, to know the destination for UDP sends
			// ie. we extract ip and port from the information and add it to our McUdpSendDataHandler
			participant->partInfoListener->connectUdp(top, udpSendDataHandler);

			return udpSendDataHandler;
		}
		else if(top.getTransport() == Topic::TRANSPORT_TCP)
		{
			SendDataHandler* newSendDataHandler = new TCPSendDataHandler(top, participant->getIOService());
			sendDataHandlers[key] = newSendDataHandler;
			return newSendDataHandler;
		}
		else
		{
			return NULL;
		}
	}

	void SendDataHandlerFactory::releaseSendDataHandler(Topic& top, Participant* participant)
	{
		SafeLock lock(&mutex);
		
		if(top.getTransport() == Topic::TRANSPORT_UDP) {
			participant->partInfoListener->disconnectUdp(top, udpSendDataHandler);
		}

///TODO
	}

}
