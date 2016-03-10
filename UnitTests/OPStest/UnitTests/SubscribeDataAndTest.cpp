// PizzaTest.cpp : Defines the entry point for the console application.
//
#include "SubscribeDataAndTest.h"
#include "gtest/gtest.h"
#include "TestPizza.cpp"



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
	virtual void CreateSubscriber(ops::Participant* part, std::string topicName) = 0;
	virtual void DeleteSubscriber(bool doLog = true) = 0;
	virtual void StartSubscriber() = 0;
	virtual void StopSubscriber() = 0;
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
		DeleteSubscriber(false);
	}



	void CreateSubscriber(ops::Participant* part, std::string topicName)
	{
		if (sub) {
			std::cout << "Subscriber already exist for topic " << sub->getTopic().getName() << std::endl;
		} else {
			try {
				//Create topic, might throw ops::NoSuchTopicException
				ops::Topic topic = part->createTopic(topicName);

				//Create a subscriber on that topic.
				sub = new DataTypeSubscriber(topic);
				sub->addDataListener(this);
				sub->deadlineMissedEvent.addDeadlineMissedListener(this);

				sub->start();
			}
			catch (...) {
				std::cout << "Requested topic '" << topicName << "' does not exist!!" << std::endl;
			}
		}
	}

	void DeleteSubscriber(bool doLog = true)
	{
		if (sub) {
			std::cout << "Deleting subscriber for topic " << sub->getTopic().getName() << std::endl;
			sub->stop();
			delete sub;
			sub = NULL;
		} else {
			if (doLog) std::cout << "Subscriber must be created first!!" << std::endl;
		}
	}

	void StartSubscriber()
	{
		if (sub) {
			std::cout << "Starting subscriber for topic " << sub->getTopic().getName() << std::endl;
			sub->start();
		} else {
			std::cout << "Subscriber must be created first!!" << std::endl;
		}

	}

	void StopSubscriber()
	{
		if (sub) {
			std::cout << "Stoping subscriber for topic " << sub->getTopic().getName() << std::endl;
			sub->stop();
		} else {
			std::cout << "Subscriber must be created first!!" << std::endl;
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

	///Override from ops::DataListener, called whenever new data arrives.
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

	///Override from ops::DeadlineMissedListener, called if no new data has arrived within deadlineQoS.
	void onDeadlineMissed(ops::DeadlineMissedEvent* evt)
	{
		std::cout << "Deadline Missed for topic " << sub->getTopic().getName() << std::endl;
	}
};


std::vector<pizza::special::ExtraAllt> receivedPizzaVec;

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
ItemInfo* itemInfo;

class MyListener :

	public CHelperListener<pizza::special::ExtraAllt>
{
public:

	void onData(ops::Subscriber* sub, pizza::special::ExtraAllt* data){
		receivedPizzaVec.push_back(*data);
		std::cout << "GETS DATA, size = " << receivedPizzaVec.size() << std::endl;
	}
};


//test class
class Test_OPS_Publisher_And_Subscriber : public testing::Test
{
public:
	Test_OPS_Publisher_And_Subscriber() {}

    ~Test_OPS_Publisher_And_Subscriber() {}
};


int main(int argc, char**argv)
{

	// --------------------------------------------------------------------
	MyListener myListener;

	itemInfo = new ItemInfo("PizzaDomain", "ExtraAlltTopic", "pizza.special.ExtraAllt");
	// Setup the OPS static error service (common for all participants, reports errors during participant creation)
	ops::ErrorWriter* errorWriterStatic = new ops::ErrorWriter(std::cout);
	ops::Participant::getStaticErrorService()->addListener(errorWriterStatic);

	// Create participants
	// NOTE that the second parameter (participantID) must be different for the two participant instances
	ops::Participant* participant = ops::Participant::getInstance("PizzaDomain", "PizzaDomain");
	if (participant == NULL) {
	    std::cout << "Failed to create Participant. Missing ops_config.xml ??" << std::endl;
		exit(-1);
	}
	participant->addTypeSupport(new PizzaProject::PizzaProjectTypeFactory());
	ops::Participant* otherParticipant = ops::Participant::getInstance("OtherPizzaDomain", "OtherPizzaDomain");
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

	// Finish up our ItemInfo's
	itemInfo->helper = (new TExtraAlltHelper(&myListener));
	itemInfo->part = participant;
	itemInfo->selected = true;
	itemInfo->helper->CreateSubscriber(itemInfo->part, itemInfo->TopicName);
	itemInfo->helper->StartSubscriber();
	ops::TimeHelper::sleep(22000); //listen on data for 22 seconds

	//delete objects

	delete itemInfo->helper;
	itemInfo->helper = NULL;
	itemInfo->part = NULL;
	delete itemInfo;
	itemInfo = NULL;
	//listen for ten seconds

	participant->getErrorService()->removeListener(errorWriter);
	otherParticipant->getErrorService()->removeListener(errorWriter2);

	delete errorWriter; errorWriter = NULL;
	delete errorWriter2; errorWriter2 = NULL;

	///TODO this should be done by asking Participant to delete instances??
	delete participant;

    //run unit tests
    ::testing::InitGoogleTest(&argc, argv);
    int result = RUN_ALL_TESTS();

    return result;
}


TEST_F(Test_OPS_Publisher_And_Subscriber, TESTING_NORMAL_EXTRA_ALLT) {
	EXPECT_EQ(receivedPizzaVec.size(), 22);
	test::testExtraAlltNormal(receivedPizzaVec.at(0), NORMAL);
}

TEST_F(Test_OPS_Publisher_And_Subscriber, TESTING_LARGE_EXTRA_ALLT) {
	test::testExtraAlltLarge(receivedPizzaVec.at(1), NORMAL);
}

TEST_F(Test_OPS_Publisher_And_Subscriber, TESTING_BURST_NORMAL_EXTRA_ALLT) {
	for(int i = 2; i < 12; ++i) {
		test::testExtraAlltNormal(receivedPizzaVec.at(i), NORMAL);
	}
}

TEST_F(Test_OPS_Publisher_And_Subscriber, TESTING_BURST_LARGE_EXTRA_ALLT) {
	for(int i = 12; i < 22; ++i) {
		test::testExtraAlltLarge(receivedPizzaVec.at(i), NORMAL);
	}
}

