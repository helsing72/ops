#ifdef NOT_USED_ANYMORE

#include "OPSTypeDefs.h"
#include "SendDiscoveryModule.h"
#include "OPSConstants.h"
#include "ByteBuffer.h"


namespace ops
{

    SendDiscoveryModule::SendDiscoveryModule(std::string domainAddress) : mcReceiver(domainAddress, ops::constants::DISCOVERY_PORT), keepRunning(false)
    {
    }

    SendDiscoveryModule::~SendDiscoveryModule()
    {
        //delete interfaceSubscriber;
    }

    void SendDiscoveryModule::run()
    {
        //Let action take place here....
        keepRunning = true;
        while (keepRunning)
        {
            if (mcReceiver.available() > 0)
            {
                char* bytes = new char[ops::constants::MAX_SIZE];
                ByteBuffer buf(bytes);
                mcReceiver.receive(bytes, ops::constants::MAX_SIZE);
                if (buf.CheckHeader())
                {
                    TopicInterfaceData* data = (TopicInterfaceData*) buf.ReadOPSObject(&helper);
                    updateInterfaces(*data);
                    delete data;
                }
                delete[] bytes;
            }
            else
            {
                TimeHelper::sleep(1);
            }
        }
    }

    void SendDiscoveryModule::updateInterfaces(TopicInterfaceData interfac)
    {
        removeInactiveInterfaces();
        for (unsigned int i = 0; i < topicInterfaces.size(); i++)
        {
            if (topicInterfaces[i].dataEquals(interfac))
            {
                topicInterfaces[i].feedWatchdog();
                return;
            }
        }
        boost::mutex::scoped_lock lock(mutex);
        topicInterfaces.push_back(interfac);
        std::cout << "Added subscriber for " << interfac.topicName << "\n";

    }

    /*void SendDiscoveryModule::onNewData(DataNotifier* subscriber)
    {
         updateInterfaces(*interfaceSubscriber->GetData());

    }
    void SendDiscoveryModule::onDeadlineMissed(DeadlineMissedEvent* event )
    {
        removeInactiveInterfaces();

    }*/
    void SendDiscoveryModule::removeInactiveInterfaces()
    {

        for (unsigned int i = 0; i < topicInterfaces.size(); i++)
        {
            if (!topicInterfaces[i].isActive())
            {
                std::vector<TopicInterface>::iterator p = topicInterfaces.begin();
                p += i;
                boost::mutex::scoped_lock lock(mutex);
                std::cout << "Removed subscriber for " << topicInterfaces[i].getData().topicName << "\n";
                topicInterfaces.erase(p);
            }

        }
    }
}

#endif // NOT_USED_ANYMORE
