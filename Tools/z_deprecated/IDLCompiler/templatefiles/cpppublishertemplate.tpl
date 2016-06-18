#ifndef __underscoredPackName__classNamePublisher_h
#define __underscoredPackName__classNamePublisher_h

#include "Publisher.h"
#include "Topic.h"
#include "Transport.h"
#include "OPSObject.h"
#include "__className.h"
#include "__classNameHelper.h"
#include <string>

namespace __packageDeclaration
{


class __classNamePublisher : public ops::Publisher 
{
    
public:
    __classNamePublisher(ops::Topic<__className> t)
        : ops::Publisher(ops::Topic<>(t.GetName(), t.GetPort(), t.GetTypeID(), t.GetDomainAddress()))
    {
        setObjectHelper(new __classNameHelper());   

    }
    
    ~__classNamePublisher(void)
    {
    }
    
    void write(__className* data)
    {
        ops::Publisher::write(data);

    }
    ops::AckData writeReliable(__className* data, std::string destinationIdentity)
    {
        return ops::Publisher::writeReliable(data, destinationIdentity);

    }
    

};

}
__packageCloser

#endif