#ifndef ops_SendDataHandlerFactory_h
#define ops_SendDataHandlerFactory_h

#include <map>

#include "OPSTypeDefs.h"
#include "Topic.h"
#include "Lockable.h"

namespace ops
{
    class SendDataHandler;
    class Participant;

    class SendDataHandlerFactory
    {
    private:
		Participant* participant;
        SendDataHandler* udpSendDataHandler;

        std::map<InternalKey_T, SendDataHandler*> sendDataHandlers;

        Lockable mutex;

    public:
        explicit SendDataHandlerFactory(Participant* part);
		// Make sure all SendDataHandlers are released before freeing the instance 
 	    ~SendDataHandlerFactory();

        SendDataHandler* getSendDataHandler(Topic& top, Participant& participant);
        void releaseSendDataHandler(Topic& top, Participant& participant);
    };

}

#endif
