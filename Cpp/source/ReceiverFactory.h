#ifndef ops_ReceiverFactory_h
#define ops_ReceiverFactory_h

#include "Receiver.h" 
#include "Topic.h"
#include "Participant.h"

namespace ops
{

    class ReceiverFactory
    {
    public:
        //Creates a receiver based on topic transport information and ioService
        static Receiver* getReceiver(Topic& top, Participant& participant);

    };
}
#endif
