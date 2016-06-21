#ifndef __className_h
#define __className_h

#include "ops.h"
#include "__packageNameTopicConfig.h"

//Request
#include "__requestNoPackage.h"
#include "__requestNoPackageSubscriber.h"
#include "__requestNoPackagePublisher.h"

//Reply
#include "__replyNoPackage.h"
#include "__replyNoPackagePublisher.h"
#include "__replyNoPackageSubscriber.h"

#include <string>

namespace __packageName
{

class __classNameStub: ops::DataListener
{
private:
	__replyTypePublisher* pub;
	__requestTypeSubscriber* sub;
	ops::KeyFilterQoSPolicy* keyQoS;
	long long replyDelay;

        void onNewData(ops::DataNotifier* subscriber)
	{
		__replyType data;
		data = handleRequest(((__requestTypeSubscriber*)subscriber)->GetData());
		Sleep(replyDelay);
		pub->Write(&data);
	}

public:
        ///Constructor - Must be called by subclasses
	__classNameStub(std::string domainAddress, std::string key)
		: replyDelay(10)
	{
                __topicConfigClass topicConfig(domainAddress);
		sub = new __requestTypeSubscriber(topicConfig.get__requestTopic());
		keyQoS = new ops::KeyFilterQoSPolicy(key);
		sub->addFilterQoSPolicy(keyQoS);
		sub->addDataListener(this);
		sub->Start();

		pub = new __replyTypePublisher(topicConfig.get__replyTopic());
		pub->setKey(key);
	}
        ///Destructor - 
	virtual ~__classNameStub()
	{
            delete sub;
            delete pub;
	}
        ///The pure virtual function to be overriden by subclasses that implements the stub.
	virtual __replyType handleRequest(__requestType* arg) = 0;
	
};

class __classNameProxy
{
private:
	__replyTypeSubscriber* sub;
	__requestTypePublisher* pub;
	ops::KeyFilterQoSPolicy* keyQoS;
	int nrOfResends;
	long long replyTimeout;

public:
	__classNameProxy(std::string domainAddress)
	  : replyTimeout(500),
		nrOfResends(5)
	{
		__topicConfigClass topicConfig(domainAddress);
		sub = new __replyTypeSubscriber(topicConfig.get__replyTopic());
		keyQoS = new ops::KeyFilterQoSPolicy("defkey");
		sub->addFilterQoSPolicy(keyQoS);
		sub->Start();

		pub = new __requestTypePublisher(topicConfig.get__requestTopic());
		

	}

	__replyType request(__requestType* arg, std::string key)
	{
		//Remove old new data events
		sub->waitForNewData(1);
		pub->setKey(key);
		keyQoS->setKey(key);
		__replyType retData;

		for (int i = 0; i < nrOfResends; i++)
		{
			pub->Write(arg);

			if(sub->waitForNewData(replyTimeout))
			{
				sub->GetData(&retData);

				//Here we should validate data.

				return retData;
			}

		}
		return retData;


	}

	~__classNameProxy()
	{
		delete sub;
		delete pub;
	}


};


}

#endif