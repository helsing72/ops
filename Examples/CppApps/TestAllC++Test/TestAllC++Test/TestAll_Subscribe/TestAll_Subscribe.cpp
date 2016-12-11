// TestAll_Subscribe.cpp : Defines the entry point for the console application.
//
#ifdef _WIN32
  #include <Windows.h>
#else
  #include <stdlib.h>
#endif

#include <ops.h>
#include "TestAll/ChildDataSubscriber.h"
#include "TestAll/BaseDataSubscriber.h"
#include "TestAll/TestAllTypeFactory.h"
#include <iostream>
#include <vector>
#include "Receiver.h"

//Create a class to act as a listener for OPS data and deadlines
class Main : ops::DataListener, ops::DeadlineMissedListener, ops::Listener<ops::BytesSizePair>
{
public:
	//Use a member subscriber so we can use it from onNewData, see below.
	TestAll::ChildDataSubscriber* sub;
	TestAll::BaseDataSubscriber* baseSub;
	TestAll::ChildData data;
	ops::Subscriber* piSub;

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
	Main(bool piSubscriber): 
	  packagesLost(0), 
	  lastPacket(-1),
	  sub(NULL), baseSub(NULL), piSub(NULL), rec(NULL)
	{
		using namespace TestAll;
		using namespace ops;


		//Create a topic. NOTE, this is a temporary solution to get topics before OPS4 is completely released.
		//ops::Topic<ChildData> topic("ChildTopic", 6778, "testall.ChildData", "236.7.8.44");

		//Create a topic from configuration.
		ops::Participant* participant = Participant::getInstance("TestAllDomain");
		participant->addTypeSupport(new TestAll::TestAllTypeFactory());

		/*rec = ops::Receiver::createTCPClient("127.0.0.1", 1342, participant->getIOService());
		rec->addListener(this);
		rec->asynchWait(bytes, 100);
		while(true)
		{
			Sleep(1000);
		}*/

		ErrorWriter* errorWriter = new ErrorWriter(std::cout);
		participant->addListener(errorWriter);

		if (!piSubscriber) {
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

			//I godtycklig component
			Topic baseTopic = participant->createTopic("BaseTopic");

			//Create a subscriber on that topic.
			baseSub = new BaseDataSubscriber(baseTopic);
			baseSub->setDeadlineQoS(22000);		
			baseSub->addDataListener(this);
			baseSub->deadlineMissedEvent.addDeadlineMissedListener(this);
			baseSub->start();

		} else {
			Topic piTopic = participant->createParticipantInfoTopic();
			piSub = new ops::Subscriber(piTopic);
			piSub->addDataListener(this);
			piSub->start();
		}

		//while(true)
		//{
		//	Sleep(1000);
		//	sub->aquireMessageLock();
		//	onNewData(sub);
		//	sub->releaseMessageLock();
		//}
	}
	///Override from ops::DataListener, called whenever new data arrives.
	void onNewData(ops::DataNotifier* subscriber)
	{
		
		if(subscriber == sub)
		{
			
			sub->numReservedMessages();

///LA test
			sub->getData(&data);   
//			ops::OPSMessage* newMess = sub->getMessage();
//			data = *(TestAll::ChildData*)newMess->getData();
///LA

			//std::cout << data.i << " From: " << sub->getMessage()->getPublisherName() << std::endl;


			//data = (TestAll::ChildData*)sub->getMessage()->getData();
			//for(int i = 0; i < data->fs.size(); i++)
			//{
			//	if(i != (int)data->fs[i])
			//	{
			//		throw "Error!";
			//	}

			//}

			////Do this to tell OPS not to delete this message until you do unreserve() on it, note you must keep track of your reserved references to avoid memory leak.
			//ops::OPSMessage* newMess = sub->getMessage();
			//newMess->reserve();
			//inCommingMessages.push_back(newMess);

			////When we have 50 samples in our list, we print, remove and unreserve them. This will allow their memory to be freed.
			//if(inCommingMessages.size() == 5)
			//{
			//	for(unsigned int i = 0; i < inCommingMessages.size(); i++)
			//	{
			//		std::cout << inCommingMessages[i]->getPublicationID() << " From: " << inCommingMessages[i]->getPublisherName() << std::endl;
			//		inCommingMessages[i]->unreserve();

			//	}
			//	inCommingMessages.clear();

			//}
			//If you dont want to keep track of data yourself, you can use the history deque from the subscriber, its max size is set by sub->setHistoryMaxSize() in constructor.
			//std::cout << "Buffer size: " << sub->getHistory().size() << std::endl;

			
			
			//if(data == NULL) return;
			if(data.i != (lastPacket + 1))
			{
				packagesLost++;
			}
			lastPacket = data.i;
			std::cout << data.baseText << " "  << " " << sub->getMessage()->getPublicationID() 
				<< " From: " << sub->getMessage()->getPublisherName()
				<< " ds[0] = " << data.ds.at(0) 
				<< ". Lost messages: " << packagesLost << "          \r";// << std::endl;
		
		}
		else if (subscriber == baseSub)
		{
			//Sleep(100000);
			TestAll::BaseData* data;
			data = (TestAll::BaseData*)baseSub->getMessage()->getData();
			if(data == NULL) return;
			std::cout << std::endl << data->baseText << " " << baseSub->getMessage()->getPublicationID() << " From: " << baseSub->getMessage()->getPublisherName() << std::endl;
		}
		else if (subscriber == piSub) 
		{
			ops::ParticipantInfoData* data = (ops::ParticipantInfoData*)piSub->getMessage()->getData();
			if (data == NULL) return;
			//std::string name;
			//std::string id;
			//std::string domain;
			//std::string ip;
			std::cout << "name: " << data->name << ", id: " << data->id << ", domain: " << data->domain << ", ip: " << data->ip << std::endl;
			//std::string languageImplementation;
			//std::string opsVersion;
			//int mc_udp_port;
			//int mc_tcp_port;
			std::cout << "  lang: " << data->languageImplementation << ", opsver: " << data->opsVersion << ", mcudp: " << data->mc_udp_port << ", mctcp: " << data->mc_tcp_port << std::endl;
			//std::vector<TopicInfoData> subscribeTopics;
			std::cout << "  subscr Topics: ";
			for (unsigned int i = 0; i < data->subscribeTopics.size(); i++) {
				std::cout << data->subscribeTopics[i].name << " ";
			}
			std::cout << std::endl;
			//std::vector<TopicInfoData> publishTopics;
			std::cout << "  pub Topics: ";
			for (unsigned int i = 0; i < data->publishTopics.size(); i++) {
				std::cout << data->publishTopics[i].name << " ";
			}
			std::cout << std::endl;
			//std::vector<std::string> knownTypes;
			std::cout << "  knownTypes: ";
			for (unsigned int i = 0; i < data->knownTypes.size(); i++) {
  				std::cout << data->knownTypes[i] << " ";
			}
			std::cout << std::endl;
		}
	}
	///Override from ops::DeadlineMissedListener, called if no new data has arrived within deadlineQoS.
	void onDeadlineMissed(ops::DeadlineMissedEvent* evt)
	{
		std::cout << "Deadline Missed!" << std::endl;
	}
	~Main()
	{
		if (baseSub) baseSub->stop();
		if (baseSub) delete baseSub;
		if (sub) sub->stop();
		if (sub) delete sub;
		if (piSub) piSub->stop();
		if (piSub) delete piSub;
	}

};

//Application entry point
int main(int argc, char* args)
{
	ops::Participant* participant = ops::Participant::getInstance("TestAllDomain");
	
	bool asPiSub = (argc > 1); 
	
	//Create an object that will listen to OPS events
	Main* m = new Main(asPiSub);

	//Make sure the OPS ioService never runs out of work.
	//Run it on main application thread only.
	for(int i = 0; i < 100; )
	{
		Sleep(100);
		//break;
		/*m.sub->aquireMessageLock();
		std::deque<ops::OPSMessage*> messages = m.sub->getHistory();
		for(int i = 0; i < 5 && i < messages.size(); i++)
		{
			std::cout << messages[i]->getPublicationID() << " ";
		}
		m.sub->releaseMessageLock();
		std::cout << std::endl;*/

		//ops::Participant::getIOService()->run();
	}


	delete m;
	//Force OPS shudown.
	delete participant;
	
	
	return 0;
}

