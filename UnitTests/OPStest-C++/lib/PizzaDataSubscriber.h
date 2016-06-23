#ifndef pizzaPizzaDataSubscriber_h
#define pizzaPizzaDataSubscriber_h

#include "Subscriber.h"
#include "Topic.h"
#include "OPSObject.h"
#include "PizzaData.h"


namespace pizza {



class PizzaDataSubscriber : public ops::Subscriber
{

public:
    PizzaDataSubscriber(ops::Topic t)
        : ops::Subscriber(t)
    {

    }

    bool getData(PizzaData* d)
    {
        if(!data) return false;
        aquireMessageLock();
		data->fillClone(d);
		releaseMessageLock();
        return true;
    }

    PizzaData* getTypedDataReference()
    {
        return (PizzaData*)getDataReference();
    }

    ~PizzaDataSubscriber(void)
    {

    }
private:
    PizzaData narrowedData;

};


}


#endif
