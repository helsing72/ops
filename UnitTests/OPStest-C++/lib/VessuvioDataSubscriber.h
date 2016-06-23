#ifndef pizzaVessuvioDataSubscriber_h
#define pizzaVessuvioDataSubscriber_h

#include "Subscriber.h"
#include "Topic.h"
#include "OPSObject.h"
#include "VessuvioData.h"


namespace pizza {



class VessuvioDataSubscriber : public ops::Subscriber
{

public:
    VessuvioDataSubscriber(ops::Topic t)
        : ops::Subscriber(t)
    {

    }

    bool getData(VessuvioData* d)
    {
        if(!data) return false;
        aquireMessageLock();
		data->fillClone(d);
		releaseMessageLock();
        return true;
    }

    VessuvioData* getTypedDataReference()
    {
        return (VessuvioData*)getDataReference();
    }

    ~VessuvioDataSubscriber(void)
    {

    }
private:
    VessuvioData narrowedData;

};


}


#endif
