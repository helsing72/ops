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
    
    
    __className* GetData()
    {
        return &narrowedData;
    }
    bool GetData(__className* d)
    {
        bool ret = firstDataReceived;
        ops::SafeLock lock(this);
        *d = narrowedData;
        return ret;
    }
    __className getData()
    {
        ops::SafeLock lock(this);
        return narrowedData;
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
