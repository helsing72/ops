//Auto generated OPS-code. DO NOT MODIFY!
#ifndef __packageName___classNameH
#define __packageName___classNameH

#include <string>

#include "KeyFilterQoSPolicy.h"
#include "OPSFactory.h"

__includes

namespace __packageName
{
    ///Factory for creating __projectName IDL-Project specific Publishers and Subscribers
    class __className : public ops::OPSFactory
    {
        std::string domainAddress;
        public:

        ///Default empty constructor
        __className()
        {

        }

        ///Constructor taking and setting domainAddress of this factory.
        __className(std::string domainAddr)
        {
            this->domainAddress = domainAddr;
        }

        ///Sets the domainAddress of this factory.
        void setDomainAddress(std::string domainAddr)
        {
            this->domainAddress = domainAddr;
        }

        ///Creates a Subscriber* for the specified topicName, returns NULL if no Subscriber exists for topicName.
        ops::Subscriber* createSubscriber(std::string topicName, std::string keyFilter = "", int deadlineQoS = 999999999, int timebasedQoS = 0)
        {
            __topicConfig topicConfig(domainAddress);

__createSubscriberBody

            if(sub != NULL)
            {
                sub->setDeadlineQoS(deadlineQoS);
                sub->setTimeBasedFilterQoS(timebasedQoS);
                if(keyFilter != "")
                {
                    sub->addFilterQoSPolicy(new ops::KeyFilterQoSPolicy(keyFilter));
                }
            }
            return sub;
        }

        ///Creates a Publisher* for the specified topicName, returns NULL if no Publisher exists for topicName.
        ops::Publisher* createPublisher(std::string topicName, std::string publisherName = "")
        {
            __topicConfig topicConfig(domainAddress);

__createPublisherBody

            if(pub != NULL)
            {
                pub->setName(publisherName);
            }
            return pub;
        }

    };

}



#endif