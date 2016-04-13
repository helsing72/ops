#ifndef pizza_specialCheeseSubscriber_h
#define pizza_specialCheeseSubscriber_h

#include "Subscriber.h"
#include "Topic.h"
#include "OPSObject.h"
#include "Cheese.h"


namespace pizza { namespace special {



class CheeseSubscriber : public ops::Subscriber
{

public:
    CheeseSubscriber(ops::Topic t)
        : ops::Subscriber(t)
    {

    }

    bool getData(Cheese* d)
    {
        if(!data) return false;
        aquireMessageLock();
        ops::Subscriber::getData()->fillClone(d);
        releaseMessageLock();
        return true;
    }

    Cheese* getTypedDataReference()
    {
        return (Cheese*)getDataReference();
    }

    ~CheeseSubscriber(void)
    {

    }

private:
    //Cheese narrowedData;

};


}}


#endif
