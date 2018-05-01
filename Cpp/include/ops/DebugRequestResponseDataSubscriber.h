#ifndef ops_DebugRequestResponseDataSubscriber_h
#define ops_DebugRequestResponseDataSubscriber_h

#include "Subscriber.h"
#include "Topic.h"
#include "OPSObject.h"
#include "DebugRequestResponseData.h"


namespace ops {


class DebugRequestResponseDataSubscriber : public ops::Subscriber
{
public:
    DebugRequestResponseDataSubscriber(ops::Topic t)
        : ops::Subscriber(t)
    {
    }

    // Copies the latest received data into d
    // Clears the "new data" flag (see newDataExist()).
    bool getData(DebugRequestResponseData* d)
    {
        if(!data) return false;
        aquireMessageLock();
        getTypedDataReference()->fillClone(d);
        releaseMessageLock();
        return true;
    }

    // Copies the latest received data into d
    // Clears the "new data" flag (see newDataExist()).
    bool getData(DebugRequestResponseData& d)
    {
        if(!data) return false;
        aquireMessageLock();
        getTypedDataReference()->fillClone(&d);
        releaseMessageLock();
        return true;
    }

    // Returns a reference to the latest received data object.
    // Clears the "new data" flag (see newDataExist()).
    // NOTE: MessageLock should be held while working with the data object, to
    // prevent a new incoming message to delete the current one while in use.
    DebugRequestResponseData* getTypedDataReference()
    {
        return dynamic_cast<DebugRequestResponseData*>(getDataReference());
    }

    ~DebugRequestResponseDataSubscriber(void)
    {
    }

};

}


#endif
