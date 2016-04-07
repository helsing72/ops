#ifndef __underscoredPackName__classNameSubscriber_h
#define __underscoredPackName__classNameSubscriber_h

#include "Subscriber.h"
#include "Topic.h"
#include "OPSObject.h"
#include "__className.h"


__packageDeclaration



class __classNameSubscriber : public ops::Subscriber
{

public:
    __classNameSubscriber(ops::Topic t)
        : ops::Subscriber(t)
    {

    }

    bool getData(__className* d)
    {
        if(!data) return false;
        aquireMessageLock();
        ops::Subscriber::getData()->fillClone(d);
        releaseMessageLock();
        return true;
    }

    __className* getTypedDataReference()
    {
        return (__className*)getDataReference();
    }

    ~__classNameSubscriber(void)
    {

    }

private:
    //__className narrowedData;

};


__packageCloser

#endif
