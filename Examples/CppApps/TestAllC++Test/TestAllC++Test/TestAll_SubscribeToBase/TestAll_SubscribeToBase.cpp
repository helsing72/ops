// TestAll_Subscribe.cpp : Defines the entry point for the console application.
//
#ifdef _WIN32
  #include <Windows.h>
#else
  #include <stdlib.h>
#endif
#include <ops.h>
#include "OPSTypeDefs.h"
#include "TestAll/ChildDataSubscriber.h"
#include "TestAll/BaseDataSubscriber.h"
#include "TestAll/TestAllTypeFactory.h"
#include "TestAll/BaseDataPublisher.h"
#include <iostream>
#include <vector>

#include "../../../ConfigFileHelper.h"

class BaseTypeFactory : public ops::SerializableFactory
{
public:
    virtual ops::Serializable* create(const ops::TypeId_T& type) override
    {
		if (type == "TestAll.BaseData") {
			return new TestAll::BaseData();
		}
		return nullptr;
    }

	BaseTypeFactory() = default;
	~BaseTypeFactory() = default;
	BaseTypeFactory(BaseTypeFactory const&) = delete;
	BaseTypeFactory(BaseTypeFactory&&) = delete;
	BaseTypeFactory& operator =(BaseTypeFactory&&) = delete;
	BaseTypeFactory& operator =(BaseTypeFactory const&) = delete;
};

//Create a class to act as a listener for OPS data and deadlines
class Main : ops::DataListener, ops::DeadlineMissedListener
{
public:
	ops::Subscriber* baseSub = nullptr;
	TestAll::BaseDataPublisher* basePub = nullptr;
	long counter = 0;

public:
	Main(ops::FileName_T const configFile)
	{
		using namespace TestAll;
		using namespace ops;

		//Create a topic from configuration.
		ops::Participant* const participant = Participant::getInstance("TestAllDomain", "TestAllDomain", configFile);
		participant->addTypeSupport(new BaseTypeFactory());

		ErrorWriter* const errorWriter = new ErrorWriter(std::cout);
		participant->addListener(errorWriter);

		Topic topic = participant->createParticipantInfoTopic();

		//Create a BaseSubscriber on that topic.
		baseSub = new Subscriber(topic);
		//baseSub->addFilterQoSPolicy(new KeyFilterQoSPolicy("key1"));
		baseSub->setDeadlineQoS(10000);		
		baseSub->addDataListener(this);
		baseSub->deadlineMissedEvent.addDeadlineMissedListener(this);
		baseSub->start();

		//basePub = new BaseDataPublisher(topic);
		//basePub->setName("BasePublisher");

	}
	///Override from ops::DataListener, called whenever new data arrives.
	virtual void onNewData(ops::DataNotifier* subscriber) override
	{
		UNUSED(subscriber);
		counter++;

		/*TestAll::BaseData* data;
		data = (TestAll::BaseData*)baseSub->getMessage()->getData();
		if(data == nullptr) return;
		std::cout << data->baseText << " " << baseSub->getMessage()->getPublicationID() << " From: " << baseSub->getMessage()->getPublisherName() << std::endl;
		data->setKey("relay");
		basePub->write(data);*/
		std::cout << "Data received! " << counter << std::endl;

		//std::cout << ((ops::ParticipantInfoData*)baseSub->getMessage()->getData())->ips[0] << ":" <<((ops::ParticipantInfoData*)baseSub->getMessage()->getData())->mc_udp_port << std::endl;
	}
	///Override from ops::DeadlineMissedListener, called if no new data has arrived within deadlineQoS.
	virtual void onDeadlineMissed(ops::DeadlineMissedEvent* evt) override
	{
		UNUSED(evt);
		std::cout << "Deadline Missed!" << std::endl;
	}
	~Main()
	{
	}
	Main() = default;
	Main(Main const&) = delete;
	Main(Main&&) = delete;
	Main& operator =(Main&&) = delete;
	Main& operator =(Main const&) = delete;
};

//Application entry point
int main(const int argc, const char* args[])
{
	UNUSED(argc);
	UNUSED(args);
    
    setup_alt_config("Examples/OPSIdls/TestAll/ops_config.xml");

    //Add support for our types from TestAll IDL project.
	//ops::OPSObjectFactory::getInstance()->add(new TestAll::TestAllTypeFactory()); 

	ops::FileName_T configFile = "";
	if (argc > 1) { configFile = args[1]; }
	
	//Create an object that will listen to OPS events
	Main const m(configFile);

	//This is a way to create inline subscriber event handlers in c++
	/*class DataCallback : ops::DataListener 
	{ 
		void onNewData(ops::DataNotifier* subscriber)
		{
			newData();
		}
	};*/
	//DataCallback callBack;
	
	//Make sure the OPS ioService never runs out of work.
	//Run it on main application thread only.
	while(true) {
		ops::TimeHelper::sleep(1000);
	}
	return 0;
}

