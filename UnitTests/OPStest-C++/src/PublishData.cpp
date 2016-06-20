// PizzaTest.cpp : Defines the entry point for the console application.
//
#include "PublishData.h"

#undef USE_MESSAGE_HEADER

#ifndef _WIN32
#include <time.h>



__int64 getNow()
{
    struct timespec ts;
    memset(&ts, 0, sizeof(ts));
    //clock_gettime(CLOCK_REALTIME, &ts);
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

template <class DataType>
class CHelperListener
{
public:
	virtual void onData(ops::Subscriber* sub, DataType* data) = 0;
};

class IHelper
{
public:
	virtual void CreatePublisher(ops::Participant* part, std::string topicName) = 0;
	virtual void DeletePublisher(bool doLog = true) = 0;
	virtual void StartPublisher() = 0;
	virtual void StopPublisher() = 0;
	virtual void Write() = 0;
	virtual void SetDeadlineQos(__int64 timeoutMs) = 0;
	virtual ~IHelper() {};
};

template <class DataType, class DataTypePublisher, class DataTypeSubscriber>
class CHelper : public IHelper, ops::DataListener, ops::DeadlineMissedListener
{
private:
	CHelperListener<DataType>* client;
	ops::Publisher* pub;
	ops::Subscriber* sub;
	__int64 expectedPubId;

public:
	DataType data;

	CHelper(CHelperListener<DataType>* client):
		pub(NULL), sub(NULL), expectedPubId(-1)
	{
		this->client = client;
	}

	virtual ~CHelper()
	{
		DeletePublisher(false);
	}

	void CreatePublisher(ops::Participant* part, std::string topicName)
	{
		if (pub) {
			std::cout << "Publisher already exist for topic " << pub->getTopic().getName() << std::endl;
		} else {
			try {
				//Create topic, might throw ops::NoSuchTopicException
				ops::Topic topic = part->createTopic(topicName);

				//Create a publisher on that topic
				pub = new DataTypePublisher(topic);

				std::ostringstream myStream;
#ifdef _WIN32
				myStream << " Win(" << _getpid() << ")" << std::ends;
#else
				myStream << " Linux(" << getpid() << ")" << std::ends;
#endif
				pub->setName("C++Test " + myStream.str());
			}
			catch (...) {
				std::cout << "Requested topic '" << topicName << "' does not exist!!" << std::endl;
			}
		}
	}

	void DeletePublisher(bool doLog = true)
	{
		if (pub) {
			std::cout << "Deleting publisher for topic " << pub->getTopic().getName() << std::endl;
			//pub->stop();
			delete pub;
			pub = NULL;
		} else {
			if (doLog) std::cout << "Publisher must be created first!!" << std::endl;
		}
	}

	void StartPublisher()
	{
		if (pub) {
			pub->start();
		} else {
			std::cout << "Publisher must be created first!!" << std::endl;
		}
	}

	void StopPublisher()
	{
		if (pub) {
			pub->stop();
		} else {
			std::cout << "Publisher must be created first!!" << std::endl;
		}
	}

	void Write()
	{
		if (pub) {
			pub->writeOPSObject(&data);
		} else {
			std::cout << "Publisher must be created first!!" << std::endl;
		}
	}

	void SetDeadlineQos(__int64 timeoutMs)
	{
		if (sub) {
			std::cout << "Setting deadlineQos to " << timeoutMs << " [ms] for topic " << sub->getTopic().getName() << std::endl;
			sub->setDeadlineQoS(timeoutMs);
		} else {
			std::cout << "Subscriber must be created first!!" << std::endl;
		}
	}

	void onNewData(ops::DataNotifier* subscriber)
	{
		if(subscriber == sub)
		{
			// Check if we have lost any messages. We use the publicationID and that works as long as
			// it is the same publisher sending us messages.
			ops::OPSMessage* newMess = sub->getMessage();

			if (expectedPubId >= 0) {
				if (expectedPubId != newMess->getPublicationID()) {
					std::cout << ">>>>> Lost message for topic " << sub->getTopic().getName() <<
						". Exp.pubid: " << expectedPubId << " got: " << newMess->getPublicationID() << std::endl;
				}
			}
			expectedPubId = newMess->getPublicationID() + 1;
			client->onData(sub, (DataType*)newMess->getData());
		}
	}

	void onDeadlineMissed(ops::DeadlineMissedEvent* evt)
	{
		std::cout << "Deadline Missed for topic " << sub->getTopic().getName() << std::endl;
	}

};

typedef CHelper<pizza::PizzaData, pizza::PizzaDataPublisher, pizza::PizzaDataSubscriber> TPizzaHelper;
typedef CHelper<pizza::VessuvioData, pizza::VessuvioDataPublisher, pizza::VessuvioDataSubscriber> TVessuvioHelper;
typedef CHelper<pizza::special::ExtraAllt, pizza::special::ExtraAlltPublisher, pizza::special::ExtraAlltSubscriber> TExtraAlltHelper;

struct ItemInfo {
	std::string Domain;
	std::string TopicName;
	std::string TypeName;

	bool selected;
	IHelper* helper;
	ops::Participant* part;

	ItemInfo(std::string dom, std::string top, std::string typ)
	{
		Domain = dom;
		TopicName = top;
		TypeName = typ;
		helper = NULL;
		part = NULL;
		selected = false;
	};
};

std::vector<ItemInfo*> ItemInfoList;


class MyListener :
		public CHelperListener<pizza::VessuvioData>,
		public CHelperListener<pizza::special::ExtraAllt>
{
public:
	void onData(ops::Subscriber* sub, pizza::special::ExtraAllt* data){}
	void onData(ops::Subscriber* sub, pizza::VessuvioData* data){}
};

int main(int argc, char**argv)
{

	// --------------------------------------------------------------------
	MyListener myListener;

	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "ExtraAlltTopic"	  , "pizza.special.ExtraAllt"));
	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "TcpExtraAlltTopic", "pizza.special.ExtraAllt"));
	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "UdpExtraAlltTopic", "pizza.special.ExtraAllt"));

	// Setup the OPS static error service (common for all participants, reports errors during participant creation)
	ops::ErrorWriter* errorWriterStatic = new ops::ErrorWriter(std::cout);
	ops::Participant::getStaticErrorService()->addListener(errorWriterStatic);

	// Create participants
	// NOTE that the second parameter (participantID) must be different for the two participant instances
	ops::Participant* participant = ops::Participant::getInstance("PizzaDomain", "PizzaDomain", "UnitTests/OPStest-C++/ops_config.xml");
	if (participant == NULL) {
	    std::cout << "Failed to create Participant. Missing ops_config.xml ??" << std::endl;
		exit(-1);
	}
	participant->addTypeSupport(new PizzaProject::PizzaProjectTypeFactory());

	ops::Participant* otherParticipant = ops::Participant::getInstance("OtherPizzaDomain", "OtherPizzaDomain", "UnitTests/OPStest-C++/ops_config.xml");
	if (otherParticipant == NULL) {
		std::cout << "Failed to create Participant. Missing ops_config.xml ??" << std::endl;
        exit(-1);
	}
	otherParticipant->addTypeSupport(new PizzaProject::PizzaProjectTypeFactory());

	// Add error writers to catch internal ops errors
	ops::ErrorWriter* errorWriter = new ops::ErrorWriter(std::cout);
	participant->getErrorService()->addListener(errorWriter);

	ops::ErrorWriter* errorWriter2 = new ops::ErrorWriter(std::cout);
	otherParticipant->getErrorService()->addListener(errorWriter2);




	//init pizza data to be sent

	//normal
	pizza::special::ExtraAllt extraAlltLarge;
	init::initExtraAlltLarge(extraAlltLarge);
	pizza::special::ExtraAllt extraAlltNormal;
	init::initExtraAlltNormal(extraAlltNormal);

	//TCP
	pizza::special::ExtraAllt extraAlltLargeTCP;
	init::initExtraAlltLargeTCP(extraAlltLargeTCP);
	pizza::special::ExtraAllt extraAlltNormalTCP;
	init::initExtraAlltNormalTCP(extraAlltNormalTCP);

	//UDP
	pizza::special::ExtraAllt extraAlltNormalUDP;
	init::initExtraAlltNormalUDP(extraAlltNormalUDP);
	pizza::special::ExtraAllt extraAlltLargeUDP;
	init::initExtraAlltLargeUDP(extraAlltLargeUDP);

	TExtraAlltHelper* hlpExtra = NULL;


	// Finish up our ItemInfo's
	for(unsigned int i = 0; i < ItemInfoList.size(); ++i) {
		ItemInfo* itemInfo = ItemInfoList[i];
		itemInfo->helper = new TExtraAlltHelper(&myListener);


		itemInfo->part = participant;
		itemInfo->selected = true;



		itemInfo->helper->CreatePublisher(itemInfo->part, itemInfo->TopicName);
		itemInfo->helper->StartPublisher();


		if		(i == 0) std::cout << "skickar extra allt" << std::endl;
		else if	(i == 1) std::cout << "skickar extra allt TCP" << std::endl;
		else if	(i == 2) std::cout << "skickar extra allt UDP" << std::endl;



		hlpExtra = (TExtraAlltHelper*)itemInfo->helper;
		if		(i == 0) hlpExtra->data = extraAlltNormal;
		else if (i == 1) hlpExtra->data = extraAlltNormalTCP;
		else if (i == 2) hlpExtra->data = extraAlltNormalUDP;

		ops::TimeHelper::sleep(2000);

		itemInfo->helper->Write();
		std::cout << "should write first data" << std::endl;
		ops::TimeHelper::sleep(1000);

		if		(i == 0) hlpExtra->data = extraAlltLarge;
		else if (i == 1) hlpExtra->data = extraAlltLargeTCP;
		else if (i == 2) hlpExtra->data = extraAlltLargeUDP;

		itemInfo->helper->Write();

		std::cout << "should write second data" << std::endl;
		ops::TimeHelper::sleep(1000);

		if		(i == 0) hlpExtra->data = extraAlltNormal;
		else if (i == 1) hlpExtra->data = extraAlltNormalTCP;
		else if (i == 2) hlpExtra->data = extraAlltNormalUDP;


		std::cout << "should write 3-12 data" << std::endl;
		for(int j = 0 ; j < 10; ++j){
			itemInfo->helper->Write();
			ops::TimeHelper::sleep(10);
		}

		ops::TimeHelper::sleep(1000);

		if(i == 0) 		 hlpExtra->data = extraAlltLarge;
		else if (i == 1) hlpExtra->data = extraAlltLargeTCP;
		else if (i == 2) hlpExtra->data = extraAlltLargeUDP;

		std::cout << "should write 13-22 data" << std::endl;
		for(int k = 0 ; k < 10; ++k){
			itemInfo->helper->Write();
			ops::TimeHelper::sleep(100);
		}
	}

	for (unsigned int idx = 0; idx < ItemInfoList.size(); ++idx) {
		ItemInfo* ii = ItemInfoList[idx];
		if (ii->helper) delete ii->helper;
		ii->helper = NULL;
		ii->part = NULL;
		delete ItemInfoList[idx];
		ItemInfoList[idx] = NULL;
	}

	participant->getErrorService()->removeListener(errorWriter);
	otherParticipant->getErrorService()->removeListener(errorWriter2);
	delete errorWriter; errorWriter = NULL;
	delete errorWriter2; errorWriter2 = NULL;
	///TODO this should be done by asking Participant to delete instances??
	delete participant;
}
