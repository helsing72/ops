// TestAll_Subscribe.cpp : Defines the entry point for the console application.
//
#include <OPSTypeDefs.h>
#include <ops.h>
#include "TestAll/ChildDataSubscriber.h"
#include "TestAll/BaseDataSubscriber.h"
#include "TestAll/TestAllTypeFactory.h"
#include <iostream>
#include <vector>
#include "Receiver.h"
#include "PrintArchiverOut.h"

//Create a class to act as a listener for OPS data and deadlines

class Main : ops::DataListener, ops::DeadlineMissedListener, ops::Listener<ops::BytesSizePair>
{
public:
    //Use a member subscriber so we can use it from onNewData, see below.
    TestAll::ChildDataSubscriber* sub;
    TestAll::BaseDataSubscriber* baseSub;
    TestAll::ChildData data;

    std::vector<ops::OPSMessage*> inCommingMessages;

    //
    int packagesLost;
    int lastPacket;

    ops::Receiver* rec;
    char bytes[100];
public:

    void onNewEvent(ops::Notifier<ops::BytesSizePair>* sender, ops::BytesSizePair byteSizePair)
    {
        rec->asynchWait(bytes, 100);
    }

    Main() :
    packagesLost(0),
    lastPacket(-1)
    {
        using namespace TestAll;
        using namespace ops;

        //Create a topic from configuration.
        ops::Participant* participant = Participant::getInstance("TestAllDomain");
        participant->addTypeSupport(new TestAll::TestAllTypeFactory());

        ErrorWriter* errorWriter = new ErrorWriter(std::cout);
        participant->addListener(errorWriter);

        Topic topic = participant->createTopic("ChildTopic");

        //Create a subscriber on that topic.
        sub = new ChildDataSubscriber(topic);
        //sub->setDeadlineQoS(10);
        //sub->setTimeBasedFilterQoS(1000);
        //sub->addFilterQoSPolicy(new KeyFilterQoSPolicy("key1"));
        sub->addDataListener(this);
        sub->deadlineMissedEvent.addDeadlineMissedListener(this);
        //sub->setHistoryMaxSize(5);
        sub->start();

        Topic baseTopic = participant->createTopic("BaseTopic");

        //Create a subscriber on that topic.
        baseSub = new BaseDataSubscriber(baseTopic);
        //baseSub->setDeadlineQoS(10000);
        baseSub->addDataListener(this);
        baseSub->deadlineMissedEvent.addDeadlineMissedListener(this);
        baseSub->start();

    }
    ///Override from ops::DataListener, called whenever new data arrives.

    void onNewData(ops::DataNotifier* subscriber)
    {

        if (subscriber == sub)
        {

            sub->numReservedMessages();

            sub->getData(&data);

            //if(data == NULL) return;
            if (data.i != (lastPacket + 1))
            {
                packagesLost++;
            }
            //lastPacket = data.i;
            //std::cout << data.baseText << " " << " " << sub->getMessage()->getPublicationID() << " From: " << sub->getMessage()->getPublisherName() << ". Lost messages: " << packagesLost << std::endl; // << std::endl;

            ops::PrintArchiverOut printer(std::cout);
            printer.printObject("childData", &data);

        }
        else
        {
            //Sleep(100000);
            TestAll::BaseData* data;
            data = (TestAll::BaseData*)baseSub->getMessage()->getData();
            if (data == NULL) return;
            //std::cout << data->baseText << " " << baseSub->getMessage()->getPublicationID() << " From: " << baseSub->getMessage()->getPublisherName() << std::endl;
            ops::PrintArchiverOut printer(std::cout);
            printer.printObject("baseData", data);
        }
    }
    ///Override from ops::DeadlineMissedListener, called if no new data has arrived within deadlineQoS.

    void onDeadlineMissed(ops::DeadlineMissedEvent* evt)
    {
        std::cout << "Deadline Missed!" << std::endl;
    }

    ~Main()
    {
        baseSub->stop();
        delete baseSub;
        sub->stop();
        delete sub;

    }

};

//Application entry point

int main(int argc, char* args)
{
    //Create an object that will listen to OPS events
    Main* m = new Main();

    //Make sure the OPS ioService never runs out of work.
    //Run it on main application thread only.
    for (int i = 0; i < 100;)
    {
        ops::TimeHelper::sleep(10000);

    }


    delete m;
    //Force OPS shudown.

    return 0;
}

