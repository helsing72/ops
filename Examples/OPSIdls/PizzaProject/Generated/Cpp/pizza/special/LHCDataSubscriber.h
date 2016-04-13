#ifndef pizza_specialLHCDataSubscriber_h
#define pizza_specialLHCDataSubscriber_h

#include "Subscriber.h"
#include "Topic.h"
#include "OPSObject.h"
#include "LHCData.h"


namespace pizza { namespace special {



class LHCDataSubscriber : public ops::Subscriber
{

public:
    LHCDataSubscriber(ops::Topic t)
        : ops::Subscriber(t)
    {

    }

    bool getData(LHCData* d)
    {
        if(!data) return false;
        aquireMessageLock();
        ops::Subscriber::getData()->fillClone(d);
        releaseMessageLock();
        return true;
    }

    LHCData* getTypedDataReference()
    {
        return (LHCData*)getDataReference();
    }

    ~LHCDataSubscriber(void)
    {

    }

private:
    //LHCData narrowedData;

};


}}


#endif
