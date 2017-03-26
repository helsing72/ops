// PizzaTest.cpp : Defines the entry point for the console application.
//

#ifdef _WIN32
#include "stdafx.h"
#include <windows.h>
#include <conio.h>
#include <process.h>
#endif

#include <stdio.h>
#include <stdlib.h>
#include <sstream>

#include <ops.h>

#include "pizza/PizzaData.h"
#include "pizza/PizzaDataSubscriber.h"
#include "pizza/PizzaDataPublisher.h"
#include "pizza/VessuvioData.h"
#include "pizza/VessuvioDataSubscriber.h"
#include "pizza/VessuvioDataPublisher.h"
#include "pizza/special/ExtraAllt.h"
#include "pizza/special/ExtraAlltSubscriber.h"
#include "pizza/special/ExtraAlltPublisher.h"

#include "PizzaProject/PizzaProjectTypeFactory.h"

#include "OPSConfigRepository.h"
#include "OPSUtilities.h"
#include "PubIdChecker.h"

#ifdef _WIN32
#include "SdsSystemTime.h"
#endif

#undef USE_MESSAGE_HEADER

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

template <class DataType>
class CHelperListener
{
public:
	virtual void onData(ops::Subscriber* sub, DataType* data) = 0;
};

class IHelper
{
public:
	virtual bool HasPublisher() = 0;
	virtual bool HasSubscriber() = 0;
	virtual void CreatePublisher(ops::Participant* part, std::string topicName) = 0;
	virtual void DeletePublisher(bool doLog = true) = 0;
	virtual void StartPublisher() = 0;
	virtual void StopPublisher() = 0;
	virtual void Write() = 0;
	virtual void CreateSubscriber(ops::Participant* part, std::string topicName) = 0;
	virtual void DeleteSubscriber(bool doLog = true) = 0;
	virtual void StartSubscriber() = 0;
	virtual void StopSubscriber() = 0;
	virtual void SetDeadlineQos(__int64 timeoutMs) = 0;
	virtual ~IHelper() {};
};

template <class DataType, class DataTypePublisher, class DataTypeSubscriber>
class CHelper : public IHelper, ops::DataListener, ops::DeadlineMissedListener, ops::Listener<ops::PublicationIdNotification_T>
{
private:
	CHelperListener<DataType>* client;
	//ops::Publisher* pub;
	DataTypePublisher* pub;
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
		DeleteSubscriber(false);
	}

	bool HasPublisher() { return pub != NULL; }
	bool HasSubscriber() { return sub != NULL; }

	void CreatePublisher(ops::Participant* part, std::string topicName)
	{
		if (pub) {
			std::cout << "Publisher already exist for topic " << pub->getTopic().getName() << std::endl;
		} else {
			try {
				//Create topic, might throw ops::NoSuchTopicException
				ops::Topic topic = part->createTopic(topicName);

				std::cout << "Created topic " << topic.getName() <<
					" [" << topic.getTransport() <<
					"::" << topic.getDomainAddress() <<
					"::" << topic.getPort() <<
					"] " << std::endl;

				std::cout <<
					"  InSocketBufferSize: " << topic.getInSocketBufferSize() << std::endl <<
					"  OutSocketBufferSize: " << topic.getOutSocketBufferSize() << std::endl <<
					"  SampleMaxSize: " << topic.getSampleMaxSize() << std::endl;

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
			//pub->writeOPSObject(&data);		// Write using pointer
			pub->write(data);					// Write using ref
		} else {
			std::cout << "Publisher must be created first!!" << std::endl;
		}
	}

	void CreateSubscriber(ops::Participant* part, std::string topicName)
	{
		if (sub) {
			std::cout << "Subscriber already exist for topic " << sub->getTopic().getName() << std::endl;
		} else {
			try {
				//Create topic, might throw ops::NoSuchTopicException
				ops::Topic topic = part->createTopic(topicName);

				std::cout << "Created topic " << topic.getName() <<
					" [" << topic.getTransport() <<
					"::" << topic.getDomainAddress() <<
					"::" << topic.getPort() <<
					"] " << std::endl;

				std::cout <<
					"  InSocketBufferSize: " << topic.getInSocketBufferSize() << std::endl <<
					"  OutSocketBufferSize: " << topic.getOutSocketBufferSize() << std::endl <<
					"  SampleMaxSize: " << topic.getSampleMaxSize() << std::endl;

				//Create a subscriber on that topic.
				sub = new DataTypeSubscriber(topic);
				sub->addDataListener(this);
				sub->deadlineMissedEvent.addDeadlineMissedListener(this);

				// Add a publication ID Checker
				sub->pubIdChecker = new ops::PublicationIdChecker;
				sub->pubIdChecker->addListener(this);

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

	virtual void onNewEvent(ops::Notifier<ops::PublicationIdNotification_T>* sender, ops::PublicationIdNotification_T arg)
	{
		UNUSED(sender);
		std::string address;
		int port;
		arg.mess->getSource(address, port);

		std::string newPub = (arg.newPublisher) ? "NEW Publisher" : "SEQ ERROR";

		std::cout << "PubIdChecker(): " << newPub << 
			", Addr: " << address <<
			", Port: " << port <<
			", Expected: " << arg.expectedPubID <<
			", Got: " << arg.mess->getPublicationID() <<
			std::endl;
	}

	///Override from ops::DataListener, called whenever new data arrives.
	void onNewData(ops::DataNotifier* subscriber)
	{
		if(subscriber == sub)
		{
			// Check if we have lost any messages. We use the publicationID and that works as long as
			// it is the same publisher sending us messages.
			ops::OPSMessage* newMess = sub->getMessage();

// Check is done with the OPS built-in PublicationIDChecker()
//			if (expectedPubId >= 0) {
//				if (expectedPubId != newMess->getPublicationID()) {
//					std::cout << ">>>>> Lost message for topic " << sub->getTopic().getName() <<
//						". Exp.pubid: " << expectedPubId << " got: " << newMess->getPublicationID() << std::endl;
//				}
//			}
			expectedPubId = newMess->getPublicationID() + 1;
			client->onData(sub, dynamic_cast<DataType*>(newMess->getData()));
		}
	}

	///Override from ops::DeadlineMissedListener, called if no new data has arrived within deadlineQoS.
	void onDeadlineMissed(ops::DeadlineMissedEvent* evt)
	{
		UNUSED(evt)
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

static bool beQuite = false;


class MyListener :
	public CHelperListener<pizza::PizzaData>,
	public CHelperListener<pizza::VessuvioData>,
	public CHelperListener<pizza::special::ExtraAllt>
{
public:
	void onData(ops::Subscriber* sub, pizza::PizzaData* data)
	{
		// test for sending derived objects on same topic
		if (dynamic_cast<pizza::VessuvioData*>(data) != NULL) {
			onData(sub, dynamic_cast<pizza::VessuvioData*>(data));
			return;
		}

		std::string addr = "";
		int port = 0;
		sub->getMessage()->getSource(addr, port);

		if (!beQuite) {
			std::cout <<
				"[Topic: " << sub->getTopic().getName() <<
				"] (From " << addr << ":" << port <<
				") PizzaData:: Cheese: " << data->cheese <<
				", Tomato sauce: " << data->tomatoSauce << 
				", spareBytes: " << data->spareBytes.size() << 
				std::endl;
#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
			std::cout << std::endl << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl << std::endl;
#endif
		}

//		pizza::PizzaData* ttt = (pizza::PizzaData*)data->clone();
//		std::cout <<
//			"Clone PizzaData:: Cheese: " << ttt->cheese <<
//			", Tomato sauce: " << ttt->tomatoSauce << 
//			", spareBytes: " << ttt->spareBytes.size() << 
//			std::endl;

	}

	void onData(ops::Subscriber* sub, pizza::VessuvioData* data)
  {
		// test for sending derived objects on same topic
		if (dynamic_cast<pizza::special::ExtraAllt*>(data) != NULL) {
			onData(sub, dynamic_cast<pizza::special::ExtraAllt*>(data));
			return;
		}

		std::string addr = "";
		int port = 0;
		sub->getMessage()->getSource(addr, port);

		if (!beQuite) {
			std::cout <<
				"[Topic: " << sub->getTopic().getName() <<
				"] (From " << addr << ":" << port <<
				") VessuvioData:: Cheese: " << data->cheese <<
				", Tomato sauce: " << data->tomatoSauce <<
				", Ham length: " << data->ham.size() << std::endl;
		}
	}

	void onData(ops::Subscriber* sub, pizza::special::ExtraAllt* data)
  {
		std::string addr = "";
		int port = 0;
		sub->getMessage()->getSource(addr, port);

		if (!beQuite) {
			std::ostringstream ss;
			if (data->shs.size() > 1) {
				ss << ", shs[1]: " << data->shs.at(1) << std::ends;
			} else {
				ss << "" << std::ends;
			}

			std::cout <<
				"[Topic: " << sub->getTopic().getName() <<
				"] (From " << addr << ":" << port <<
				") ExtraAllt:: Cheese: " << data->cheese <<
				ss.str() <<
				", Tomato sauce: " << data->tomatoSauce <<
				", Num strings: " << data->strings.size() << std::endl;
		}
	}
};

static int NumVessuvioBytes = 0;
static std::string FillerStr("");
__int64 sendPeriod = 1000;

void WriteToAllSelected()
{
	static __int64 Counter = 0;

	for (unsigned int i = 0; i < ItemInfoList.size(); i++) {
		ItemInfo* info = ItemInfoList[i];
		if (!info->selected) continue;
		std::stringstream str;
		str << Counter;
		std::string CounterStr(str.str());
		Counter++;
		if (info->TypeName == pizza::PizzaData::getTypeName()) {
			TPizzaHelper* hlp = (TPizzaHelper*)info->helper;
			hlp->data.cheese = "Pizza from C++: " + CounterStr;
			hlp->data.tomatoSauce = "Tomato";
#ifdef USE_MESSAGE_HEADER
			hlp->data.systemTime = sds::sdsSystemTime();
#endif
		}
		if (info->TypeName == pizza::VessuvioData::getTypeName()) {
			TVessuvioHelper* hlp = (TVessuvioHelper*)info->helper;
			hlp->data.cheese = "Vessuvio from C++: " + CounterStr;
			hlp->data.ham = FillerStr;
#ifdef USE_MESSAGE_HEADER
			hlp->data.systemTime = sds::sdsSystemTime();
#endif
		}
		if (info->TypeName == pizza::special::ExtraAllt::getTypeName()) {
			TExtraAlltHelper* hlp = (TExtraAlltHelper*)info->helper;
			hlp->data.cheese = "ExtraAllt from C++: " + CounterStr;
			if (hlp->data.strings.size() == 0) {
				for (int k = 0; k < 1000; k++) hlp->data.strings.push_back("hej");
			}
			hlp->data.sh = -7;
			if (hlp->data.shs.size() == 0) {
				hlp->data.shs.push_back(17);
				hlp->data.shs.push_back(42);
				hlp->data.shs.push_back(-63);
			}
#ifdef USE_MESSAGE_HEADER
			hlp->data.systemTime = sds::sdsSystemTime();
#endif
		}
		info->helper->Write();
	}
}

void printDomainInfo(ops::Participant* part)
{
	ops::Domain* dom = part->getDomain();
	std::cout << std::endl <<
		"  DomainID: " << dom->getDomainID() << std::endl <<
		"  DomainAddress: " << dom->getDomainAddress() << std::endl <<
		"  InSocketBufferSize: " << dom->getInSocketBufferSize() << std::endl <<
		"  OutSocketBufferSize: " << dom->getOutSocketBufferSize() << std::endl <<
		"  MetaDataMcPort: " << dom->getMetaDataMcPort() << std::endl <<
		"  LocalInterface: " << dom->getLocalInterface() << std::endl <<
		"  TimeToLive: " <<	dom->getTimeToLive() << std::endl;

	ops::Topic top = part->createParticipantInfoTopic();

	std::cout << std::endl <<
		"  TopicName: " << top.getName() << std::endl <<
		"  TypeID: " << top.getTypeID() << std::endl <<
		"  ParticipantID: " << top.getParticipantID() << std::endl <<
		"  DomainID: " << top.getDomainID() << std::endl <<
		"  DomainAddress: " << top.getDomainAddress() << std::endl <<
		"  LocalInterface: " << top.getLocalInterface() << std::endl <<
		"  Transport: " << top.getTransport() << std::endl <<
		"  TimeToLive: " << top.getTimeToLive() << std::endl <<
		"  SampleMaxSize: " << top.getSampleMaxSize() << std::endl <<
		"  Port: " << top.getPort() << std::endl <<
		"  InSocketBufferSize: " << top.getInSocketBufferSize() << std::endl <<
		"  OutSocketBufferSize: " << top.getOutSocketBufferSize() << std::endl;

}

void menu()
{
	std::cout << "" << std::endl;
	for (unsigned int i = 0; i < ItemInfoList.size(); i++) {
            ItemInfo* ii = ItemInfoList[i];
		std::cout << "\t " << i <<
			" " <<
		(ii->helper->HasPublisher() ? "P" : " ") <<
		(ii->helper->HasSubscriber() ? "S" : " ") <<
		(ii->selected ? "*" : " ") <<
			" " <<
		ii->Domain << "::" << ii->TopicName << std::endl;
	}

	std::cout << "" << std::endl;
	std::cout << "\t PC    Create Publishers" << std::endl;
	std::cout << "\t PD    Delete Publishers" << std::endl;
	std::cout << "\t PS    Start Publishers" << std::endl;
	std::cout << "\t PT    Stop Publishers" << std::endl;
	std::cout << "\t SC    Create Subscriber" << std::endl;
	std::cout << "\t SD    Delete Subscriber" << std::endl;
	std::cout << "\t SS    Start Subscriber" << std::endl;
	std::cout << "\t ST    Stop Subscriber" << std::endl;
	std::cout << "\t L num Set num Vessuvio Bytes [" << NumVessuvioBytes << "]" << std::endl;
	std::cout << "\t T ms  Set deadline timeout [ms]" << std::endl;
	std::cout << "\t V ms  Set send period [ms] [" << sendPeriod << "]" << std::endl;
	std::cout << "\t A     Start/Stop periodical Write with set period" << std::endl;
	std::cout << "\t W     Write data" << std::endl;
	std::cout << "\t Q     Quite (minimize program output)" << std::endl;
	std::cout << "\t X     Exit program" << std::endl;

#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
	std::cout << std::endl << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl << std::endl;
#endif

}

int main(int argc, char**argv)
{
	UNUSED(argc)
	UNUSED(argv)
//	for (int i = 0; i < 10; i++) {
//		std::cout << ".";
//		ops::TimeHelper::sleep(1000);
//	}

#ifdef _WIN32
	// --------------------------------------------------------------------
	// Try to set timer resolution to 1 ms
	#define TARGET_RESOLUTION 1         // 1-millisecond target resolution

	TIMECAPS tc;
	UINT     wTimerRes = TARGET_RESOLUTION;

	if (timeGetDevCaps(&tc, sizeof(TIMECAPS)) == TIMERR_NOERROR) {
		wTimerRes = min(max(tc.wPeriodMin, TARGET_RESOLUTION), tc.wPeriodMax);
	}
	timeBeginPeriod(wTimerRes);

	sds::sdsSystemTimeInit();
#endif

	// Setup the OPS static error service (common for all participants, reports errors during participant creation)
	ops::ErrorWriter* errorWriterStatic = new ops::ErrorWriter(std::cout);
	ops::Participant::getStaticErrorService()->addListener(errorWriterStatic);

	// Add all Domain's from given file(s)
	ops::OPSConfigRepository::Instance()->Add("ops_config.xml");
//	ops::OPSConfigRepository::Instance()->Add("ops_config2.xml");

	// Add a specific domain from a file
//	ops::OPSConfigRepository::Instance()->Add("ops_config.xml", "PizzaDomain");
//	ops::OPSConfigRepository::Instance()->Add("ops_config2.xml", "OtherPizzaDomain");

	

	// --------------------------------------------------------------------
	MyListener myListener;

	// Setup the InfoItem list (TODO take from ops_config.xml)
	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "PizzaTopic", "pizza.PizzaData"));
	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "VessuvioTopic", "pizza.VessuvioData"));

	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "PizzaTopic2", "pizza.PizzaData"));
	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "VessuvioTopic2", "pizza.VessuvioData"));

	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "TcpPizzaTopic", "pizza.PizzaData"));
	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "TcpVessuvioTopic", "pizza.VessuvioData"));

	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "TcpPizzaTopic2", "pizza.PizzaData"));
	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "TcpVessuvioTopic2", "pizza.VessuvioData"));

	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "UdpPizzaTopic", "pizza.PizzaData"));
	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "UdpVessuvioTopic", "pizza.VessuvioData"));

	ItemInfoList.push_back(new ItemInfo("OtherPizzaDomain", "OtherPizzaTopic", "pizza.PizzaData"));
	ItemInfoList.push_back(new ItemInfo("OtherPizzaDomain", "OtherVessuvioTopic", "pizza.VessuvioData"));

	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "ExtraAlltTopic", "pizza.special.ExtraAllt"));

	ItemInfoList.push_back(new ItemInfo("PizzaDomain", "PizzaTopic", "pizza.PizzaData"));

	// Create participants
	// NOTE that the second parameter (participantID) must be different for the two participant instances
	ops::Participant* participant = ops::Participant::getInstance("PizzaDomain", "PizzaDomain");
    if (participant == NULL) {
	    std::cout << "Failed to create Participant. Missing ops_config.xml ??" << std::endl;
		exit(-1);
    }
	participant->addTypeSupport(new PizzaProject::PizzaProjectTypeFactory());
	printDomainInfo(participant);

	ops::Participant* otherParticipant = ops::Participant::getInstance("OtherPizzaDomain", "OtherPizzaDomain");
	if (otherParticipant == NULL) {
		std::cout << "Failed to create Participant. Missing ops_config.xml ??" << std::endl;
        exit(-1);
	}
	otherParticipant->addTypeSupport(new PizzaProject::PizzaProjectTypeFactory());
	printDomainInfo(otherParticipant);

	// Add error writers to catch internal ops errors
	ops::ErrorWriter* errorWriter = new ops::ErrorWriter(std::cout);
	participant->getErrorService()->addListener(errorWriter);

	ops::ErrorWriter* errorWriter2 = new ops::ErrorWriter(std::cout);
	otherParticipant->getErrorService()->addListener(errorWriter2);

	// Finish up our ItemInfo's
	for (unsigned int idx = 0; idx < ItemInfoList.size(); idx++) {
		ItemInfo* info = ItemInfoList[idx];

		// Create helper
		if (info->TypeName == pizza::PizzaData::getTypeName()) {
			info->helper = new TPizzaHelper(&myListener);
		}
		if (info->TypeName == pizza::VessuvioData::getTypeName()) {
			info->helper = new TVessuvioHelper(&myListener);
		}
		if (info->TypeName == pizza::special::ExtraAllt::getTypeName()) {
			info->helper = new TExtraAlltHelper(&myListener);
		}

		// Set participant
		if (info->Domain == "PizzaDomain") {
			info->part = participant;
		}
		if (info->Domain == "OtherPizzaDomain") {
			info->part = otherParticipant;
		}
	}

	ItemInfo* ii = ItemInfoList[0];
	ii->selected = true;

	bool doExit = false;
	bool doPeriodicSend = false;

	menu();

	__int64 nextSendTime = (__int64)getNow() + sendPeriod;

	while (!doExit) {
		std::cout << std::endl << " (? = menu) > ";

		// Repeated sends
		if (doPeriodicSend) {
			while (!_kbhit()) {
				__int64 now = (__int64)getNow();
				if (now >= nextSendTime) {
					// write
					WriteToAllSelected();
					// Calc next time to send
					nextSendTime = now + sendPeriod;
				}
				ops::TimeHelper::sleep(1);
			}
		}

		typedef enum {NONE, PUB, SUB} TFunction;

		TFunction func = NONE;

		char buffer[1024];
		char* ptr = fgets(buffer, sizeof(buffer), stdin);
		if (ptr == NULL) continue;

		std::string line(buffer);

		// trim start
		std::string::size_type idx = line.find_first_not_of(" \t");
		if (idx == std::string::npos) continue;
		if (idx > 0) line.erase(0, idx);
		if (line.size() == 0) continue;

		char ch = line[0];
		line.erase(0, 1);

		if ((ch == 'p') || (ch == 'P')) func = PUB;
		if ((ch == 's') || (ch == 'S')) func = SUB;

		if (func != NONE) {
			if (line.size() == 0) {
				std::cout << "ERROR: Expected character after '" << ch << "'" << std::endl;
				continue;
			}
			ch = line[0];
			line.erase(0, 1);
		}

		int num = 0;
		if ((ch >= '0') && (ch <= '9')) {
			num = ch - '0';
			if (line.size() > 0) {
				ch = line[0];
				if ((ch >= '0') && (ch <= '9')) {
					num = (10 * num) + (ch - '0');
					line.erase(0, 1);
				}
			}
			ch = '0';
			if (num >= (int)ItemInfoList.size()) {
				std::cout << "ERROR: Index to large. Max = " << ItemInfoList.size() << std::endl;
				continue;
			}
		}

		// trim start
		idx = line.find_first_not_of(" \t");
		if (idx != std::string::npos) {
			if (idx > 0) line.erase(0, idx);
		}

        ii = ItemInfoList[num];

		switch (ch) {
			case '?':
				menu();
				break;

			case '0':
				ii->selected = !ii->selected;
				break;

			case 'a':
			case 'A':
				doPeriodicSend = !doPeriodicSend;
				break;

			case 'c':
			case 'C':
				for (unsigned int i = 0; i < ItemInfoList.size(); i++) {
					ItemInfo* info = ItemInfoList[i];
					if (!info->selected) continue;
					if (func == PUB) {
						info->helper->CreatePublisher(info->part, info->TopicName);
					} else if (func == SUB) {
						info->helper->CreateSubscriber(info->part, info->TopicName);
					}
				}
				break;

			case 'd':
			case 'D':
				for (unsigned int i = 0; i < ItemInfoList.size(); i++) {
					ItemInfo* info = ItemInfoList[i];
					if (!info->selected) continue;
					if (func == PUB) {
						info->helper->DeletePublisher();
					} else if (func == SUB) {
						info->helper->DeleteSubscriber();
					}
				}
				break;

			case 's':
			case 'S':
				for (unsigned int i = 0; i < ItemInfoList.size(); i++) {
					ItemInfo* info = ItemInfoList[i];
					if (!info->selected) continue;
					if (func == PUB) {
						info->helper->StartPublisher();
					} else if (func == SUB) {
						info->helper->StartSubscriber();
					}
				}
				break;

			case 't':
			case 'T':
				for (unsigned int i = 0; i < ItemInfoList.size(); i++) {
					ItemInfo* info = ItemInfoList[i];
					if (!info->selected) continue;
					if (func == PUB) {
						info->helper->StopPublisher();
					} else if (func == SUB) {
						info->helper->StopSubscriber();
					} else {
						__int64 timeout = atoi(line.c_str());
						info->helper->SetDeadlineQos(timeout);
					}
				}
				break;

			case 'l':
			case 'L':
				{
					num = atoi(line.c_str());
					if (num >= 0) NumVessuvioBytes = num;
					if (FillerStr.size() > (unsigned int)num) FillerStr.erase(num);
					while (FillerStr.size() < (unsigned int)num) FillerStr += " ";
					std::cout << "Length: " << FillerStr.size() << std::endl;
				}
				break;

			case 'v':
			case 'V':
				{
					num = atoi(line.c_str());
					if (num >= 0) sendPeriod = num;
					std::cout << "sendPeriod: " << sendPeriod << std::endl;
				}
				break;

			case 'q':
			case 'Q':
				beQuite = !beQuite;
				break;

			case 'w':
			case 'W':
				WriteToAllSelected();
				break;

			case 'x':
			case 'X':
				doExit = true;
				break;

			case 'y':
			case 'Y':
				break;
		}
	}

	// --------------------------------------------------------------------

	for (unsigned int idx = 0; idx < ItemInfoList.size(); idx++) {
        ii = ItemInfoList[idx];
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
/// Participant is a singleton and should not be deleted in this way!!!
///	delete participant;

#ifdef _WIN32
	// --------------------------------------------------------------------
	// We don't need the set resolution any more
	timeEndPeriod(wTimerRes);
#endif

}
