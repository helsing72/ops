// TestAll_Publish.cpp : Defines the entry point for the console application.
//

#include "TestAll/ChildDataPublisher.h"
#include "TestAll/BaseDataPublisher.h"
#include "TestAll/TestAllTypeFactory.h"
#include <ops.h>
#include <XMLArchiverOut.h>
#include <XMLArchiverIn.h>

#include <iostream>
#include <fstream>

#include "Receiver.h"
#include "Sender.h"

int main(int argc, const char* args[])
{
	int mainSleep = 100;
	int nrOfFloats = 500;
	int sleepEveryNrPackets = 100000;
	int sendSleepTime = 1;

	if(argc > 3)
	{
		sscanf(args[1],"%i", &mainSleep);
		sscanf(args[2],"%i", &nrOfFloats);
		sscanf(args[3],"%i", &sleepEveryNrPackets);
		sscanf(args[4],"%i", &sendSleepTime);
	}
	else
	{
		std::cout << "Ignoring arguments (must be > 3) arguments should be: " << "mainSleep, nrOfFloats, sleepEveryNrPackets, sendSleepTime." << std::endl;

	}

	using namespace TestAll;
	using namespace ops;

	std::vector<TestData*> bP;
	bP.push_back(new TestData());
	bP.clear();

	ops::Participant* participant = Participant::getInstance("TestAllDomain");
	participant->addTypeSupport(new TestAll::TestAllTypeFactory());

	ErrorWriter* errorWriter = new ErrorWriter(std::cout);
	participant->addListener(errorWriter);


	//Create topic, might throw ops::NoSuchTopicException
	Topic topic = participant->createTopic("ChildTopic");

	//Create a publisher on that topic
	ChildDataPublisher pub(topic);
	pub.setName("TestAllPublisher");
	pub.sendSleepTime = sendSleepTime;
	pub.sleepEverySendPacket = sleepEveryNrPackets;

	Topic baseTopic = participant->createTopic("BaseTopic");

	BaseDataPublisher basePub(baseTopic);
	basePub.setName("BasePublisher");

	//Create some data to publish
	ChildData data;
	BaseData baseData;
	baseData.baseText = "Text from base";


	//Set Base class field
	data.baseText = "Hello";

	////Set aggregated object
	TestData testData;
	testData.text = "text in aggregated class";
	testData.value = 3456.0;
	data.test2 = testData;

	data.testPointer = (TestData*)testData.clone();

	//
	//Set primitives
	data.bo = true;
	data.b = 1;
	data.i = 0;
	data.l = 3;
	data.f = 4.0;
	data.d = 5.0;
	//data.s = "World";
	data.s = "World";

	//Set arrays (vectors)
	data.bos.push_back(true);
	data.bs.push_back(6);
	//data.is.push_back(7);
	data.ls.push_back(8);
	//data.fs.push_back(9.0);
	data.ds.push_back(10.0);
	data.ss.push_back("Hello Array");
	data.setKey("key1");

	//return 0;

	for(int i = 0; i < nrOfFloats; i++)
	{
		data.fs.push_back(i);
	}



	ChildData* dataClone = (ChildData*)data.clone();

	//Publish the data peridically and make a small changes to the data.
	while(true)
	{
		std::cout << "Writing " << dataClone->i <<  std::endl;
		pub.write(dataClone);

		dataClone->i++;

		if(dataClone->i % 20 == 0)
		{
			basePub.write(&baseData);
		}

                ops::TimeHelper::sleep(mainSleep);

	}

	


	delete participant;
	return 0;
}

