
#include "OPSTypeDefs.h"
#include "ReceiverFactory.h"
#include "Domain.h"
#include "BasicError.h"
#include "NetworkSupport.h"

namespace ops
{

    Receiver* ReceiverFactory::getReceiver(Topic& top, Participant* participant)
    {
        Receiver* receiver = NULL;

        IOService* ioService = participant->getIOService();

        //This should never happen, log an internal error and return NULL;
        if (ioService == NULL)
        {
			BasicError err("ReceiverFactory", "getReceiver", "Unexpected error, ioServide == NULL");
            participant->reportError(&err);
            return receiver;
        }

		std::string localIf = doSubnetTranslation(top.getLocalInterface(), participant->getIOService());

        if (top.getTransport() == Topic::TRANSPORT_MC)
        {
            receiver = Receiver::create(top.getDomainAddress(), top.getPort(), ioService, localIf, top.getInSocketBufferSize());
        }
        else if (top.getTransport() == Topic::TRANSPORT_TCP)
        {
            receiver = Receiver::createTCPClient(top.getDomainAddress(), top.getPort(), ioService, top.getInSocketBufferSize());
        }
        else if (top.getTransport() == Topic::TRANSPORT_UDP)
        {
            receiver = Receiver::createUDPReceiver(0, ioService, localIf, top.getInSocketBufferSize());
        }
        return receiver;
    }



}
