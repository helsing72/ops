#ifndef pizza_PizzaDataPublisher_h
#define pizza_PizzaDataPublisher_h

#include "Publisher.h"
#include "Topic.h"
#include "OPSObject.h"
#include "PizzaData.h"
#include <string>

namespace pizza {



class PizzaDataPublisher : public ops::Publisher 
{
    
public:
    PizzaDataPublisher(ops::Topic t)
        : ops::Publisher(t)
    {


    }
    
    ~PizzaDataPublisher(void)
    {
    }
    
    void write(PizzaData* data)
    {
        ops::Publisher::write(data);

    }
    /*
    ops::AckData writeReliable(PizzaData* data, std::string destinationIdentity)
    {
        return ops::Publisher::writeReliable(data, destinationIdentity);

    }
    */
    

};


}


#endif