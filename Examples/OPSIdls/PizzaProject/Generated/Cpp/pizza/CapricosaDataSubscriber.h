#ifndef pizzaCapricosaDataSubscriber_h
#define pizzaCapricosaDataSubscriber_h

#include "Subscriber.h"
#include "Topic.h"
#include "OPSObject.h"
#include "CapricosaData.h"


namespace pizza {



class CapricosaDataSubscriber : public ops::Subscriber
{

public:
    CapricosaDataSubscriber(ops::Topic t)
        : ops::Subscriber(t)
    {

    }

    bool getData(CapricosaData* d)
    {
        if(!data) return false;
        aquireMessageLock();
		data->fillClone(d);
		releaseMessageLock();
        return true;
    }

    CapricosaData* getTypedDataReference()
    {
        return (CapricosaData*)getDataReference();
    }

    ~CapricosaDataSubscriber(void)
    {

    }
private:
    CapricosaData narrowedData;

};


}


#endif
