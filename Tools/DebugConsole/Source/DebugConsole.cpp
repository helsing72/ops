// DebugConsole.cpp : Defines the entry point for the console application.
//

#include "DataListener.h"
#include "Participant.h"
#include "ErrorWriter.h"
#include "OPSConfigRepository.h"
#include "TimeHelper.h"
#include "PrintArchiverOut.h"

#include "ops/DebugRequestResponseDataPublisher.h"
#include "ops/DebugRequestResponseDataSubscriber.h"
#include "ops/opsTypeFactory.h"

#ifdef USE_PIZZATEST
#include "pizza\PizzaData.h"
pizza::PizzaData testData;
#endif

ops::DebugRequestResponseData request;

class DebugListener : public ops::DataListener
{
public:
	DebugListener(ops::Participant* part) : _part(part), _sub(nullptr) {}
	~DebugListener() { removeSubscriber(); }

	void Start()
	{
		setupSubscriber();
	}

	void Stop()
	{
		removeSubscriber();
	}

	void onNewData(ops::DataNotifier* notifier)
	{
		ops::Subscriber* sub = dynamic_cast<ops::Subscriber*> (notifier);
		if (sub) {
			ops::DebugRequestResponseData* data = dynamic_cast<ops::DebugRequestResponseData*>(sub->getMessage()->getData());
			if (data) {
				// Skip All requests
				if (data->Command != 0) return;

				// Only responses come here

				ops::PrintArchiverOut prt(std::cout);

				switch (data->Entity) {
				case 0: // Debug
					if ((data->Result1 == 2) || (data->Result1 == 3)) {
						if (data->Result1 == 2) {
							std::cout << "Publisher Topics:" << std::endl;
						} else {
							std::cout << "Subscriber Topics:" << std::endl;
						}
						for (unsigned int i = 0; i < data->Param3.size(); i++) {
							std::cout << "    " << data->Param3[i] << std::endl;
						}
						std::cout << std::endl;
					}
					break;

				case 1: // Participant
					break;

				case 2: // Publisher
					data->serialize(&prt);
					break;

				case 3: // Subscriber
					data->serialize(&prt);
					break;
				}

			} else {
				std::cout << "DebugListener::onNewData(), Data could not be cast as expected." << std::endl;
			}
		} else {
			std::cout << "DebugListener::onNewData(), Subscriber could not be cast as expected." << std::endl;
		}
	}

private:
	bool setupSubscriber()
	{
		_sub = new ops::Subscriber(_part->createDebugTopic());
		_sub->addDataListener(this);
		_sub->start();
		return true;
	}

	void removeSubscriber()
	{
		if (_sub) delete _sub;
		_sub = nullptr;
	}

	ops::Participant* _part;
	ops::Subscriber* _sub;

	ops::DebugRequestResponseData _request;
};

void CommandLoop(ops::Participant* part)
{
	request.setKey("Pizza");
	request.Entity = 2;	// Publisher
	request.Name = "PizzaTopic";
	request.Command = 1;
	request.Param1 = 1;

	ops::Topic top = part->createDebugTopic();
	ops::DebugRequestResponseDataPublisher pub(top);
	pub.start();
	pub.write(request);

	ops::TimeHelper::sleep(1000);
}

void Usage()
{
	std::cout << std::endl;
	std::cout << "Version 2018-05-01" << std::endl;
	std::cout << std::endl;
	std::cout << "  Usage:" << std::endl;
	std::cout << "    DebugConsole [-?] -k key -e n -n name -c cmd -p1 n" << std::endl;
	std::cout << std::endl;
	std::cout << "    -? | -h         Help" << std::endl;
	std::cout << "    -k key          Key to set in sent debug message" << std::endl;
	std::cout << "    -e type         Entity type (2 = Publisher, 3 = Subscriber)" << std::endl;
	std::cout << "    -n name         Entity name (for pub/sub the topic name)" << std::endl;
	std::cout << "    -c num          Command to send to entity" << std::endl;
	std::cout << "    -p1 num         Parameter 1" << std::endl;
	std::cout << std::endl;
}


int main(int argc, char* argv[])
{
	bool printUsage = false;
	std::string key = "";
	std::string* strp = nullptr;
	int* intp = nullptr;
	int64_t* int64p = nullptr;

	for (int i = 1; i < argc; i++) {
		std::string arg = argv[i];
		if (arg == "?") {
			printUsage = true;

		} else if ((arg == "-?") || (arg == "-h")) {
			printUsage = true;

		} else if (arg == "-c") {
			intp = &request.Command;

		} else if (arg == "-e") {
			intp = &request.Entity;

		} else if (arg == "-k") {
			strp = &key;

		} else if (arg == "-n") {
			strp = &request.Name;

		} else if (arg == "-p1") {
			int64p = &request.Param1;

#ifdef USE_PIZZATEST
		} else if (arg == "-pizza") {
			testData.cheese = "Hello from DebugConsole";
			request.Objs.push_back(&testData);
#endif
		}

		if ((strp != nullptr) && ((i+1) < argc)) {
			i++;
			*strp = argv[i];
			strp = nullptr;
		}
		if ((intp != nullptr) && ((i + 1) < argc)) {
			i++;
			*intp = atoi(argv[i]);
			intp = nullptr;
		}
		if ((int64p != nullptr) && ((i + 1) < argc)) {
			i++;
			*int64p = atoll(argv[i]);
			int64p = nullptr;
		}
	}

	request.setKey(key);

	if (printUsage) {
		Usage();
		ops::PrintArchiverOut prt(std::cout);
		request.serialize(&prt);

	} else {
		// Setup the OPS static error service (common for all participants, reports errors during participant creation)
		ops::ErrorWriter* errorWriterStatic = new ops::ErrorWriter(std::cout);
		ops::Participant::getStaticErrorService()->addListener(errorWriterStatic);

		// Add all Domain's from given file(s)
		ops::OPSConfigRepository::Instance()->Add("ops_config.xml");

		// Create participant
		ops::Participant* participant = ops::Participant::getInstance("PizzaDomain", "PizzaDomain");
		if (participant == NULL) {
			std::cout << "Failed to create Participant. Missing ops_config.xml ??" << std::endl;
			exit(-1);
		}
		participant->addTypeSupport(new ops::opsTypeFactory());

		DebugListener listener(participant);
		listener.Start();

//		CommandLoop(participant);
	
		ops::Topic top = participant->createDebugTopic();
		ops::DebugRequestResponseDataPublisher pub(top);
		pub.start();
		pub.write(request);

		request.Objs.clear();

		ops::TimeHelper::sleep(1000);
	}
    return 0;
}

