#ifndef pizza_specialExtraAlltSubscriber_h
#define pizza_specialExtraAlltSubscriber_h

#include "Subscriber.h"
#include "Topic.h"
#include "OPSObject.h"
#include "ExtraAllt.h"


namespace pizza { namespace special {



class ExtraAlltSubscriber : public ops::Subscriber
{

public:
    ExtraAlltSubscriber(ops::Topic t)
        : ops::Subscriber(t)
    {

    }

    bool getData(ExtraAllt* d)
    {
        if(!data) return false;
        aquireMessageLock();
		data->fillClone(d);
		releaseMessageLock();
        return true;
    }

    ExtraAllt* getTypedDataReference()
    {
        return (ExtraAllt*)getDataReference();
    }

    ~ExtraAlltSubscriber(void)
    {

    }
private:
    ExtraAllt narrowedData;

};


}}


#endif
