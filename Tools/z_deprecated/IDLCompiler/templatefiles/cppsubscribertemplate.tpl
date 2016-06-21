#ifndef __underscoredPackName__classNameSubscriber_h
#define __underscoredPackName__classNameSubscriber_h

#include "Subscriber.h"
#include "Topic.h"
#include "Transport.h"
#include "OPSObject.h"
#include "__className.h"
#include "__classNameHelper.h"

namespace __packageDeclaration
{


class __classNameSubscriber : public ops::Subscriber
{

public:
    __classNameSubscriber(ops::Topic<__className> t)
        : ops::Subscriber(ops::Topic<>(t.GetName(), t.GetPort(), t.GetTypeID(), t.GetDomainAddress()))
    {
        setObjectHelper(new __classNameHelper());


    }

    bool getData(__className* d)
    {
        bool ret = firstDataReceived;
        ops::SafeLock lock(this);
        hasUnreadData = false;
        *d = narrowedData;
        return ret;
    }
    __className getDataCopy()
    {
        ops::SafeLock lock(this);
        hasUnreadData = false;
        return narrowedData;
    }

    ops::OPSObject* getDataReference()
    {
        hasUnreadData = false;
        return &narrowedData;
    }

    ~__classNameSubscriber(void)
    {

    }
private:
    __className narrowedData;
protected:
    //Override
    void saveCopy(ops::OPSObject* o)
    {
        ops::SafeLock lock(this);
        narrowedData = *((__className*)o);
    }


};
}

__packageCloser

#endif
