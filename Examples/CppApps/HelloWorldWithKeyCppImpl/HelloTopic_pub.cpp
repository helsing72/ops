
//Include ops
#include <ops.h>
//Include a publisher for the data type we want to publish, generated from our IDL project HelloWorld.
#include "hello/HelloDataPublisher.h"
//Include type support for the data types we have defined in our IDL project, generated from our IDL project HelloWorld.
#include "HelloWorld/HelloWorldTypeFactory.h"
//Include iostream to get std::cout
#include <iostream>
//Include windows to get Sleep()
#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#include <stdlib.h>
#endif

int main(const int argc, const char* args[])
{
	UNUSED(argc);
	UNUSED(args);
	using namespace ops;

	//Create a Participant (i.e. an entry point for using ops.), compare with your ops_config.xml
	ops::Participant* const participant = Participant::getInstance("HelloDomain");
	if(!participant)
	{
		std::cout << "Create participant failed. do you have ops_config.xml on your rundirectory?" << std::endl;
#ifdef _WIN32
		Sleep(10000); exit(1);
#else
                usleep(10000000); exit(1);
#endif
	}

	//Add type support for our types, to make this participant understand what we are talking
	participant->addTypeSupport(new HelloWorld::HelloWorldTypeFactory());

	//Now, create the Topic we wish to publish on. Might throw ops::NoSuchTopicException if no such Topic exist in ops_config.xml
	Topic const topic = participant->createTopic("HelloTopic");

	//Create a publisher on that topic
	hello::HelloDataPublisher pub(topic);
	pub.setName("HelloTopicPublisher"); //Optional identity of the publisher

	//Create some data to publish, this is our root object.
	hello::HelloData data;

	data.helloString = "Hello World From C++!!";

	data.setKey("cpp_sample");

	//Publish the data peridically 
	int const mainSleep = 1000;
	while(true)
	{
		pub.write(&data);
		std::cout << "Writing data"  <<  std::endl;
#ifdef _WIN32
		Sleep(mainSleep);
#else
                usleep(mainSleep*1000);
#endif		
	}

	return 0;
}

