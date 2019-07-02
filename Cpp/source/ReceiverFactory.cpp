
#include "OPSTypeDefs.h"
#include "ReceiverFactory.h"
#include "Domain.h"
#include "BasicError.h"
#include "NetworkSupport.h"

namespace ops
{
    Receiver* ReceiverFactory::getReceiver(Topic& top, Participant* participant)
    {
        Receiver* receiver = nullptr;

        IOService* ioService = participant->getIOService();

        //This should never happen, log an internal error and return nullptr;
        if (ioService == nullptr)
        {
			BasicError err("ReceiverFactory", "getReceiver", "Unexpected error, ioServide == nullptr");
            participant->reportError(&err);
            return receiver;
        }

		Address_T localIf = doSubnetTranslation(top.getLocalInterface(), participant->getIOService());

        if (top.getTransport() == Topic::TRANSPORT_MC)
        {
            receiver = Receiver::create(top.getDomainAddress(), top.getPort(), ioService, localIf, top.getInSocketBufferSize());
        }
//Moved into TCPReceiveDataHandler
//        else if (top.getTransport() == Topic::TRANSPORT_TCP)
//        {
//            receiver = Receiver::createTCPClient(top.getDomainAddress(), top.getPort(), ioService, top.getInSocketBufferSize());
//        }
        else if (top.getTransport() == Topic::TRANSPORT_UDP)
        {
            int port = 0;
            // If UDP topic is configured with my node address, we use the configured port, otherwise
            // we use port 0 which will force the OS to create a unique port that we listen to.
            if (isMyNodeAddress(top.getDomainAddress(), ioService)) {
                localIf = top.getDomainAddress();
                port = top.getPort();
            }
            receiver = Receiver::createUDPReceiver(port, ioService, localIf, top.getInSocketBufferSize());
        }
        return receiver;
    }
}
