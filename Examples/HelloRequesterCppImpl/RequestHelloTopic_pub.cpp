// TestAll_Publish.cpp : Defines the entry point for the console application.
//


#include <ops.h>
#include "RequestReply.h"
#include "HelloRequestReply/HelloRequestReplyTypeFactory.h"
#include <iostream>
#include <windows.h>


int main(int argc, const char* args[])
{
using namespace ops;

//Create a Participant (i.e. an entry point for using ops.), compare with your ops_config.xml
ops::Participant* participant = Participant::getInstance("HelloDomain");
if(!participant)
{
	std::cout << "Create participant failed. do you have ops_config.xml on your rundirectory?" << std::endl;
	Sleep(10000); exit(1);
}

//Add type support for our types, to make this participant understand what we are talking
participant->addTypeSupport(new HelloRequestReply::HelloRequestReplyTypeFactory());

//Now, create the Topic we wish to publish on. Might throw ops::NoSuchTopicException if no such Topic exist in ops_config.xml
Topic requestTopic = participant->createTopic("RequestHelloTopic");
Topic replyTopic = participant->createTopic("HelloTopic");

ops::RequestReply<hello::RequestHelloData, hello::HelloData> requestReplyHelper(requestTopic, replyTopic, "req_rep_instance1");

while(true)
{
	hello::HelloData* reply = NULL;
	hello::RequestHelloData request;
	request.requestersName = "C++ Requester";

	reply = requestReplyHelper.request(&request, 1000);

	if(reply != NULL)
	{
		std::cout << "Reply received: " << reply->helloString  <<  std::endl;

		delete reply;
	}
	else
	{
		std::cout << "No reply." << std::endl;
	}
	Sleep(1000);
}

	return 0;
}

