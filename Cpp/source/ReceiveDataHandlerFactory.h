#ifndef ops_ReceiveDataHandlerFactory_h
#define ops_ReceiveDataHandlerFactory_h

#include "UDPReceiver.h"

namespace ops
{
    //Forward declaration..
    class ReceiveDataHandler;

    class ReceiveDataHandlerFactory
    {
    private:
        ///By Singelton, one ReceiveDataHandler per Topic (name) on this Participant
        std::map<std::string, ReceiveDataHandler*> receiveDataHandlerInstances;

        ///Garbage vector for ReceiveDataHandlers, these can safely be deleted.
        std::vector<ReceiveDataHandler*> garbageReceiveDataHandlers;
        ops::Lockable garbageLock;

		inline std::string makeKey(Topic& top);

    public:
        ReceiveDataHandlerFactory(Participant* participant);
        ReceiveDataHandler* getReceiveDataHandler(Topic& top, Participant* participant);
        void cleanUpReceiveDataHandlers();
        void releaseReceiveDataHandler(Topic& top, Participant* participant);
		bool cleanUpDone();
        ~ReceiveDataHandlerFactory();

    };

}

#endif
