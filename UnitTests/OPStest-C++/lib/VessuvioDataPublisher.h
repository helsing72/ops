#ifndef pizza_VessuvioDataPublisher_h
#define pizza_VessuvioDataPublisher_h

#include "Publisher.h"
#include "Topic.h"
#include "OPSObject.h"
#include "VessuvioData.h"
#include <string>

namespace pizza {



class VessuvioDataPublisher : public ops::Publisher 
{
    
public:
    VessuvioDataPublisher(ops::Topic t)
        : ops::Publisher(t)
    {


    }
    
    ~VessuvioDataPublisher(void)
    {
    }
    
    void write(VessuvioData* data)
    {
        ops::Publisher::write(data);

    }
    /*
    ops::AckData writeReliable(VessuvioData* data, std::string destinationIdentity)
    {
        return ops::Publisher::writeReliable(data, destinationIdentity);

    }
    */
    

};


}


#endif