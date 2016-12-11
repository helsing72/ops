// TestAll_Publish.cpp : Defines the entry point for the console application.
//

#ifdef _WIN32
  #include <Windows.h>
  #include <conio.h>
#else
  #include <stdlib.h>
#endif
#include <math.h>

//Include a publisher for the data type we want to publish, generated from our IDL project TestAll.
#include "TestAll/ChildDataPublisher.h"
#include "TestAll/BaseDataPublisher.h"
//Include type support for the data types we have defined in our IDL project, generated from our IDL project TestAll.
#include "TestAll/TestAllTypeFactory.h"
#include <ops.h>
#include <XMLArchiverOut.h>
#include <XMLArchiverIn.h>

//Include iostream to get std::cout
#include <iostream>
#include <fstream>

#include "Receiver.h"
#include "Sender.h"

#ifndef _WIN32
#include <sys/types.h>
#include <unistd.h>
#include <time.h>

__int64 getNow()
{
    struct timespec ts;
    memset(&ts, 0, sizeof(ts));
    clock_gettime(CLOCK_REALTIME, &ts);
    return ((1000 * ts.tv_sec) + (ts.tv_nsec / 1000000));
}

#include <stdio.h>
#include <sys/select.h>
#include <sys/ioctl.h>
#include <termios.h>

int _kbhit() {
    static const int STDIN = 0;
    static bool initialized = false;

    if (! initialized) {
        // Use termios to turn off line buffering
        termios term;
        tcgetattr(STDIN, &term);
        term.c_lflag &= ~ICANON;
        tcsetattr(STDIN, TCSANOW, &term);
        setbuf(stdin, NULL);
        initialized = true;
    }

    int bytesWaiting;
    ioctl(STDIN, FIONREAD, &bytesWaiting);
    return bytesWaiting;
}
#else
__int64 getNow()
{
    return (__int64)timeGetTime();
}
#endif

int main(int argc, const char* args[])
{
	int mainSleep = 40;
	int nrOfFloats = 500;
	int sleepEveryNrPackets = 100000;
	int sendSleepTime = 1;

	if(argc > 3)
	{
#ifdef _WIN32
		sscanf_s(args[1],"%i", &mainSleep);
		sscanf_s(args[2],"%i", &nrOfFloats);
		sscanf_s(args[3],"%i", &sleepEveryNrPackets);
		sscanf_s(args[4],"%i", &sendSleepTime);
#else
		sscanf(args[1],"%i", &mainSleep);
		sscanf(args[2],"%i", &nrOfFloats);
		sscanf(args[3],"%i", &sleepEveryNrPackets);
		sscanf(args[4],"%i", &sendSleepTime);
#endif
	}
	else
	{
		std::cout << "Ignoring arguments (must be > 3) arguments should be: " << "mainSleep, nrOfFloats, sleepEveryNrPackets, sendSleepTime." << std::endl;

	}

	
	timeBeginPeriod(1);
	using namespace TestAll;
	using namespace ops;

	std::vector<TestData*> bP;
	bP.push_back(new TestData());
	bP.clear();

	ops::Participant* participant = Participant::getInstance("TestAllDomain");
	if(!participant)
	{
		std::cout << "Create participant failed. do you have ops_config.xml on your rundirectory?" << std::endl;
		ops::TimeHelper::sleep(10000); exit(1);
	}

	//Add type support for our types, to make this participant understand what we are talking
	participant->addTypeSupport(new TestAll::TestAllTypeFactory());

	ErrorWriter* errorWriter = new ErrorWriter(std::cout);
	participant->addListener(errorWriter);

	/*Sleep(2000);
	ops::Sender* tcpServer = ops::Sender::createTCPServer("", 1342, participant->getIOService());

	while(true)
	{
		Sleep(10);
		tcpServer->sendTo("Hello World!", 13, "", 0);
	}
	return 1;*/


	//Create topic, might throw ops::NoSuchTopicException
	Topic topic = participant->createTopic("ChildTopic");

	/*{
		Topic scoopedTopic;
		scoopedTopic = participant->createTopic("ChildTopic");
		topic = scoopedTopic;
	}*/
	
	//topic.setDomainAddress("10.73.4.93");
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
	data.b = 3;
	data.sh = 16777;
	data.i = 0;
	data.l = -3;
	data.f = 4.0;
	data.d = 5.0;
	data.s = "World";

	data.fruit.value = Fruit::PEAR;

	//Set arrays (vectors)
	data.bos.push_back(true);
	data.bs.push_back(6);
	data.shs.push_back(-77);
	data.is_.push_back(7);
	data.ls.push_back(-8);
	data.fs.push_back(9.0);
	data.ds.push_back(10.0);
	data.ss.push_back("Hello Array");
	
	data.setKey("key1");

	//return 0;

	//for(int i = 0; i < nrOfFloats; i++)
	//{
	//	data.fs.push_back((float)i);
	//}

	
	/*TestData testData2;
	testData2.text = "text in aggregated array element class";
	testData2.value = 2.0;

	TestData testData3;
	testData3.text = "text in aggregated array element class";
	testData3.value = 3.0;

	data.test2s.push_back(new TestData());
	data.test2s.push_back(new TestData());

	data.test2s2.push_back(testData3);
	data.test2s2.push_back(testData2);

	std::ofstream oStream("hatt.xml");

	ops::XMLArchiverOut archiver(oStream, "config");

	archiver.inout(std::string("data"), &data);

	archiver.close();

	oStream.close();*/

	//std::ifstream iStream ("fulfile.xml");

	//ops::XMLArchiverIn archiverIn(iStream, "file");

	//
	//ops::Serializable* ser = archiverIn.inout(std::string("data"), (OPSObject*)NULL);



	//return 0;

	ChildData* dataClone = (ChildData*)data.clone();

	double theta = 0.0;
	double pi = 3.1415926535;
	double fact = 1.0;
	double dist = 0.1;
	double offset = 0.0;

	//Publish the data peridically and make a small changes to the data.
	while(true)
	{
		//std::cout << "Writing ChildTopic " << dataClone->i <<  std::endl;
		
		dataClone->i++;

		dataClone->d = (20/fact * sin(theta * pi / 180.0)) + (fact * sin(3 * theta * pi / 180.0));
		dataClone->f = (float)((30/fact * cos(theta * pi / 180.0)) + (fact * cos(3 * theta * pi / 180.0)));
		theta++;

		dataClone->fs.clear();
		dataClone->ds.clear();
		for (float v = 0; v < 600; v++)
		{
			dataClone->fs.push_back((float)(offset + (fact * sin(v * pi / 180.0))));
			dataClone->ds.push_back(-offset + (fact * cos(v * pi / 180.0)));
		}

		pub.write(dataClone);

		if(dataClone->i % 10 == 0)
		{
			basePub.write(&baseData);
		//	std::cout << "Writing BaseTopic " << std::endl;
		}

		ops::TimeHelper::sleep(mainSleep);

		if (_kbhit()) {
			char ch = _getch();
			if (ch == '+') {
				if (fact >= (15.0 * dist)) dist = 10.0 * dist; 
				fact = fact + dist;
			}
			if (ch == '-') {
				if (fact <= 1.5 * dist) dist = dist / 10.0;
				fact = fact - dist;
			}
			if (fact < dist) fact = dist;
			if (ch == '0') {
				offset = 0.0;
			}
			if (ch == '1') {
				offset = 10.0;
			}
			if (ch == '2') {
				offset = -10.0;
			}
			if (ch == 'a') {
				dataClone->ls.push_back(8);
				dataClone->ls.resize(20000);	
			}
		}
		std::cout << "offset = " << offset << ", fact = " << fact << ", dist = " << dist << std::endl;
	}

	timeEndPeriod(1);

	
	delete participant;
	return 0;
}

