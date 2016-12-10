//

#include <time.h>
#include <stdio.h>

#ifdef _WIN32
#include <direct.h>
#include <conio.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <iostream>
#include <sstream>

#ifdef _WIN32
#include <windows.h>
#else
#include <stdarg.h>
#endif

#include <ops.h>
#include "Lockable.h"
#include "ParticipantInfoData.h"

#include "COpsConfigHelper.h"
#include "SdsSystemTime.h"

#ifndef _WIN32

#define LPWSTR char*

#endif

void showDescription()
{
	std::cout << std::endl;
	std::cout << "  This program can subscribe to any OPS topic and it is possible to choose what " << std::endl;
	std::cout << "  information to present and in which order." << std::endl;
	std::cout << "  This can be used to test if / verify that topics are published." << std::endl;
	std::cout << std::endl;
}

void ShowUsage()
{
	std::cout << std::endl << "Usage:" << std::endl;
	std::cout << "  OPSListener [-v] [-?] [-c ops_cfg_file [-c ops_cfg_file [...]]]" << std::endl;
	std::cout << "              [-t] [-pA | -p<option_chars>]" << std::endl;
	std::cout << "              [-a arg_file [-a arg_file [...]]]" << std::endl;
	std::cout << "              [-IA | -I domain [-I domain [...]] [-O]]" << std::endl;
	std::cout << "              [-SA | -S domain [-S domain [...]]]" << std::endl;
	std::cout << "              [-D default_domain] Topic [Topic ...]" << std::endl;
	std::cout << std::endl;
	std::cout << "    -?                 Shows a short description" << std::endl;
	std::cout << "    -a arg_file        File with command line arguments" << std::endl;
	std::cout << "    -c ops_config_file Specifies an OPS configuration file to use" << std::endl;
	std::cout << "                       If none given, the default 'ops_config.xml' is used" << std::endl;
	std::cout << "    -D default_domain  Default domain name to use for topics given without domain name" << std::endl;
	std::cout << "                       If none given, the default 'SDSDomain' is used" << std::endl;
	std::cout << "                       A new default can be given between topics" << std::endl;
	std::cout << "    -I domain          Subscribe to Participant Info Data from given domain" << std::endl;
	std::cout << "    -IA                Subscribe to Participant Info Data from all domains in given configuration files" << std::endl;
	std::cout << "    -O                 if -I or -IA given, only show arriving and timed out participants" << std::endl;
	std::cout << "    -p<option_chars>   Defines for received messages, which fields to print and in which order" << std::endl;
	std::cout << "                 n       Publisher Name" << std::endl;
	std::cout << "                 i       Publication Id" << std::endl;
	std::cout << "                 T       Topic Name" << std::endl;
	std::cout << "                 y       Type" << std::endl;
	std::cout << "                 s       Sparebytes Size" << std::endl;
	std::cout << "                 k       Key" << std::endl;
	std::cout << "                 S       Source IP::Port" << std::endl;
	std::cout << "    -pA                Short for all option chars in the program default order" << std::endl;
	std::cout << "    -S domain          Subscribe to all topics in given domain" << std::endl;
	std::cout << "    -SA                Subscribe to all topics in all domains in given configuration files" << std::endl;
	std::cout << "    -t                 Print receive time for each message" << std::endl;
	std::cout << "    -v                 Verbose output during parsing of command line arguments" << std::endl;
	std::cout << std::endl;
	std::cout << std::endl;
}

class CArguments
{
private:
	std::string validFormatChars;
	std::map<char, bool> validChars;
	std::string indent;

	std::string toAnsi(LPWSTR wStr)
	{
#ifdef _WIN32
		// Convert current wide string to ANSI (std::string)
		char tmp[16384];
		size_t numConverted;
		wcstombs_s(&numConverted, tmp, sizeof(tmp), wStr, _TRUNCATE);
		return tmp;
#else
		return std::string(wStr);
#endif
	}

public:
	static std::string getValidFormatChars() { return "TknisyS"; }
	static std::string getDefaultDomain() { return "SDSDomain"; }

	std::vector<std::string> cfgFiles;
	std::vector<std::string> topicNames;
	std::vector<std::string> subscribeDomains;
	std::vector<std::string> infoDomains;
	std::string printFormat;
	std::string defaultDomain;
	bool verboseOutput;
	bool logTime;
	bool allInfoDomains;
	bool allTopics;
	bool onlyArrivingLeaving;

	CArguments() : indent(""), printFormat(""), defaultDomain(getDefaultDomain()),
		verboseOutput(false), logTime(false), allInfoDomains(false), allTopics(false), onlyArrivingLeaving(false)
	{
		// Create a map with all valid format chars, used for validating -p<...> argument
		validFormatChars = getValidFormatChars();
		for (unsigned int i = 0; i < validFormatChars.size(); i++) validChars[validFormatChars[i]] = true;
	}

	bool HandleArguments(LPWSTR* szArglist, int nStart, int nArgs)
	{
		if (verboseOutput) for (int i = nStart; i < nArgs; i++) {
#ifdef _WIN32
			printf("%s %d: %ws\n", indent.c_str(), i, szArglist[i]);
#else
			printf("%s %d: %s\n", indent.c_str(), i, szArglist[i]);
#endif
		}

		// Decode arguments
		int argIdx = nStart;
		while (argIdx < nArgs) {
			std::string argument = toAnsi(szArglist[argIdx++]);

			if ((argument == "?") || (argument == "-?")) {
				showDescription();
				return false;
			}

			// Command line argument file
			if (argument == "-a") {
#ifdef _WIN32
				if (argIdx >= nArgs) {
					std::cout << "Argument '-a' is missing value" << std::endl;
					return false;
				}
				if (!ParseFile(toAnsi(szArglist[argIdx++]))) return false;
#else
				std::cout << "Argument '-a' currently NOT supported on non-WIN32" << std::endl;
#endif
				continue;
			}

			// Configuration files
			if (argument == "-c") {
				if (argIdx >= nArgs) {
					std::cout << "Argument '-c' is missing value" << std::endl;
					return false;
				}
				cfgFiles.push_back(toAnsi(szArglist[argIdx++]));
				continue;
			}

			// Default domain
			if (argument == "-D") {
				if (argIdx >= nArgs) {
					std::cout << "Argument '-D' is missing value" << std::endl;
					return false;
				}
				defaultDomain = toAnsi(szArglist[argIdx++]);
				continue;
			}

			// Subscribe to Participant Info Data
			if (argument == "-I") {
				if (argIdx >= nArgs) {
					std::cout << "Argument '-I' is missing value" << std::endl;
					return false;
				}
				infoDomains.push_back(toAnsi(szArglist[argIdx++]));
				continue;
			}

			if (argument == "-IA") {
				allInfoDomains = true;
				continue;
			}

			if (argument == "-O") {
				onlyArrivingLeaving = true;
				continue;
			}

			// Subscribe to domain
			if (argument == "-S") {
				if (argIdx >= nArgs) {
					std::cout << "Argument '-S' is missing value" << std::endl;
					return false;
				}
				subscribeDomains.push_back(toAnsi(szArglist[argIdx++]));
				continue;
			}

			if (argument == "-SA") {
				allTopics = true;
				continue;
			}

			/// -p with all options
			if (argument == "-pA") {
				printFormat = getValidFormatChars();
				continue;
			}

			/// -p<...>
			if (argument.find("-p") == 0) {
				argument.erase(0, 2);
				for (unsigned int i = 0; i < argument.size(); i++) {
					if (validChars.find(argument[i]) == validChars.end()) {
						std::cout << "Argument '-p' has an invalid option char: " << argument[i] << std::endl;
						return false;
					}
				}
				printFormat = argument;
				continue;
			}

			if (argument == "-t") {
				logTime = true;
				continue;
			}

			// Verbose output or not
			if (argument == "-v") {
				verboseOutput = true;
				continue;
			}

			// The rest is topic names
			if (argument != "") {
				/// If no domain given, set the default domain
				if (COpsConfigHelper::topicName(argument) == argument) {
					argument = COpsConfigHelper::fullTopicName(defaultDomain, argument);
				}
				bool found = false;
				for (unsigned int i = 0; i < topicNames.size(); i++) {
					if (topicNames[i] == argument) {
						found = true;
						break;
					}
				}
				if (!found) topicNames.push_back(argument);
			}
		}

		return true;
	}

	///TODO for Linux
#ifdef _WIN32
	bool ParseFile(std::string fileName)
	{
		char buffer[16384];
		wchar_t wbuffer[32768];
		std::string oldIndent = indent;

		FILE* stream = NULL;
#ifdef _WIN32
		if (fopen_s(&stream, fileName.c_str(), "r") != 0) {
			std::cout << "Failed to open argument file: " << fileName << std::endl;
			return false;
		}
#else
		stream = fopen(fileName.c_str(), "r");
#endif
		if (stream == NULL) {
			std::cout << "Failed to open argument file: " << fileName << std::endl;
			return false;
		}

		indent += "  ";
		if (verboseOutput) std::cout << indent << "Parsing argument file: " << fileName << std::endl;

		bool returnValue = true;
		while (!feof(stream) && returnValue) {
			if (fgets(buffer, sizeof(buffer), stream) == NULL) break;

			int len = strlen(buffer);
			if (len == 0) continue;
			if (buffer[0] == '#') continue;

			if (buffer[len - 1] == '\n') buffer[len - 1] = ' ';

			size_t numConverted = 0;
			if (mbstowcs_s(&numConverted, wbuffer, 32768, buffer, _TRUNCATE) != 0) {
				std::cout << "mbstowcs_s() failed" << std::endl;
				returnValue = false;
				break;
			}

			LPWSTR *szArglist;
			int nArgs;

			szArglist = CommandLineToArgvW(wbuffer, &nArgs);
			if (NULL == szArglist) {
				std::cout << "CommandLineToArgvW() failed" << std::endl;
				returnValue = false;
			}
			else {
				returnValue = HandleArguments(szArglist, 0, nArgs);
			}

			// Free memory allocated for CommandLineToArgvW arguments.
			if (szArglist) LocalFree(szArglist);
		}
		fclose(stream);
		if (verboseOutput) std::cout << indent << "Finished parsing argument file: " << fileName << std::endl;
		indent = oldIndent;
		return returnValue;
	}
#endif

	bool HandleCommandLine(int argc, char* argv[])
	{
#ifdef _WIN32
		bool returnValue = false;
		int nArgs;
		LPWSTR *szArglist = CommandLineToArgvW(GetCommandLineW(), &nArgs);
		if (NULL == szArglist) {
			std::cout << "CommandLineToArgvW failed" << std::endl;
			return false;
		}
		else {
			returnValue = HandleArguments(szArglist, 1, nArgs);
		}
		// Free memory allocated for CommandLineToArgvW arguments.
		LocalFree(szArglist);
		return returnValue;
#else
		return HandleArguments(argv, 1, argc);
#endif
	}

	bool ValidateArguments()
	{
		// Validate arguments
		if ((topicNames.size() == 0) && (subscribeDomains.size() == 0) && (infoDomains.size() == 0) && !allInfoDomains && !allTopics) {
			std::cout << "No topics/domains given!!" << std::endl;
			return false;
		}

		if (printFormat == "") {
			// Set default format
			if ((topicNames.size() > 1) || (subscribeDomains.size() > 0) || allTopics) {
				printFormat = "Tisky";
			}
			else {
				printFormat = "isky";
			}
		}

		if (verboseOutput) {
			//std::vector<std::string> cfgFiles;
			std::cout << "" << std::endl;

			//std::vector<std::string> topicNames;
			std::cout << "" << std::endl;

			//std::string printFormat;
			std::cout << "  Print format:        " << printFormat << std::endl;
			std::cout << std::endl << std::endl;
		}

		return true;
	}
};

// =======================================================================================
// =======================================================================================

// Specialized factory used for receiving ANY topic as an OPSObject
class AllOpsTypeFactory : public ops::SerializableFactory
{
public:
  ops::Serializable* create(std::string& type)
  {
		if (type != "") {
			return new ops::OPSObject();
		}
		return NULL;
  }
};

class MyLogger : public ILogInterface
{
	void Log(const char* szFormatString, ...)
	{
		char buff[1000];
		memset(buff, 0, sizeof(buff));
		va_list argList;
		va_start(argList, szFormatString);
#ifdef _WIN32
		vsnprintf_s( buff, sizeof(buff), _TRUNCATE, szFormatString, argList );
#else
		vsnprintf( buff, sizeof(buff), szFormatString, argList );
#endif
		printf("%s", buff);
	}
};

// ---------------------------------------------------------------------------------------
typedef std::string (*TFormatFunc)(ops::OPSMessage* mess, ops::OPSObject* opsData);

std::string publisherName(ops::OPSMessage* mess, ops::OPSObject* opsData)
{
	if (!mess) return "";
	return "Pub: " + mess->getPublisherName();
}
std::string publicationId(ops::OPSMessage* mess, ops::OPSObject* opsData)
{
	if (!mess) return "";
	std::stringstream str;
	str << mess->getPublicationID();
	std::string IdStr(str.str());
	return "PubId: " + IdStr;
}
std::string topicName(ops::OPSMessage* mess, ops::OPSObject* opsData)
{
	if (!mess) return "";
	return "Topic: " + mess->getTopicName();
}
std::string source(ops::OPSMessage* mess, ops::OPSObject* opsData)
{
	if (!mess) return "";
	std::string srcIP;
	int srcPort;
	mess->getSource(srcIP, srcPort);
	std::ostringstream myPort;
	myPort << srcPort << std::ends;
	return "Source: " + srcIP + "::" + myPort.str();
}

// ---------------------------------------------------------------------------------------
std::string spareBytes(ops::OPSMessage* mess, ops::OPSObject* opsData)
{
	if (!opsData) return "";
	std::stringstream str;
	str << opsData->spareBytes.size();
	std::string SizeStr(str.str());
	return "Spare: " + SizeStr;
}
std::string key(ops::OPSMessage* mess, ops::OPSObject* opsData)
{
	if (!opsData) return "";
	return "Key: " + opsData->getKey();
}
std::string typeString(ops::OPSMessage* mess, ops::OPSObject* opsData)
{
	if (!opsData) return "";
	return "Type: " + opsData->getTypeString();
}

// ---------------------------------------------------------------------------------------
//Create a class to act as a listener for OPS data and deadlines
class Main : ops::DataListener
{
	bool logTime;
	MyLogger logger;
	COpsConfigHelper opsHelper;
	std::string printFormat;
	bool onlyArrivingLeaving;

	std::vector<ops::Subscriber*> vSubs;

	typedef struct {
		int64_t time;
		ops::OPSMessage* mess;
	} TEntry;
	std::deque<TEntry> List;
	ops::Lockable ListLock;

	std::map<char, TFormatFunc> formatMap;

	std::vector<std::string> ownPartInfoNames;

	std::map<std::string, int64_t> partMap;

public:
	int messDataCounter;

	int numQueued() {return (int)List.size();}

	//
	Main(CArguments& args) :
		logTime(args.logTime),
		opsHelper(&logger, &logger, args.defaultDomain),
		printFormat(args.printFormat),
		onlyArrivingLeaving(args.onlyArrivingLeaving),
		messDataCounter(0)
	{
		using namespace ops;

		// Initialize the format map
		formatMap['T'] = topicName;
		formatMap['k'] = key;
		formatMap['n'] = publisherName;
		formatMap['i'] = publicationId;
		formatMap['s'] = spareBytes;
		formatMap['y'] = typeString;
		formatMap['S'] = source;

		ErrorWriter* errorWriter = new ErrorWriter(std::cout);

		// First, Add all domains in given configuration files to the helper
		for (unsigned int i = 0; i < args.cfgFiles.size(); i++) opsHelper.DomainMapAdd(args.cfgFiles[i]);

		std::cout << std::endl;

		// Now, Check given topics and find all unique domains for these
		for (unsigned int i = 0; i < args.topicNames.size(); i++) {
			if (!opsHelper.existsTopic(args.topicNames[i])) {
				std::cout << "##### Topic '" << args.topicNames[i] << "' not found in configuration file(s)" << std::endl;
				args.topicNames[i] = "";
				continue;
			}
			opsHelper.checkTopicDomain(args.topicNames[i]);
		}

		// Handle case where we are requested to subscribe to all topics from all existing domains
		if (args.allTopics) {
			// Replace args.subscribeDomains with all known domains in configuration files
			opsHelper.getAvailableDomains(args.subscribeDomains);
		}

		// Check given subscribe domains and make sure they exist and ev. add them to the unique list
		for (unsigned int i = 0; i < args.subscribeDomains.size(); i++) {
			std::string domainName = args.subscribeDomains[i];
			if (opsHelper.existsDomain(domainName)) {
				opsHelper.checkTopicDomain(domainName + "::");
			} else {
				std::cout << "##### Domain '" << domainName << "' not found. Have you forgot configuration file(s) ?" << std::endl;
				args.subscribeDomains[i] = "";
			}
		}

		// Handle case where we are requested to listen to partition info from all existing domains
		if (args.allInfoDomains) {
			// Replace args.InfoDomains with all known domains in configuration files
			opsHelper.getAvailableDomains(args.infoDomains);
		}

		// Check given info domains and make sure they exist and ev. add them to the unique list
		for (unsigned int i = 0; i < args.infoDomains.size(); i++) {
			std::string domainName = args.infoDomains[i];
			if (opsHelper.existsDomain(domainName)) {
				opsHelper.checkTopicDomain(domainName + "::");
			} else {
				std::cout << "##### Domain '" << domainName << "' not found. Have you forgot configuration file(s) ?" << std::endl;
				args.infoDomains[i] = "";
			}
		}

		// Create a participant for each unique domain
		for (unsigned int i = 0; i < opsHelper.vDomains.size(); i++) {
			try {
				ops::Participant* part = opsHelper.getDomainParticipant(opsHelper.vDomains[i]);
				if (part == NULL) {
					std::cout << "##### Domain '" << opsHelper.vDomains[i] << "' not found. Have you forgot configuration file(s) ?" << std::endl;
					continue;
				}
				part->addTypeSupport(new AllOpsTypeFactory());
				part->addListener(errorWriter);
				ownPartInfoNames.push_back(part->getPartInfoName());
			}
			catch(...)
			{
				std::cout << "##### Domain '" << opsHelper.vDomains[i] << "' not found. Have you forgot configuration file(s) ?" << std::endl;
			}
		}

		// Now we can get the domain object for the 'subscribe domains' and add their topics to the topic list
		for (unsigned int i = 0; i < args.subscribeDomains.size(); i++) {
			std::string domainName = args.subscribeDomains[i];
			if (domainName == "") continue;

			ops::Participant* part = opsHelper.getDomainParticipant(domainName);
			if (!part) continue;
			ops::Domain* dom = part->getDomain();
			if (!dom) continue;

			std::vector<ops::Topic*> topics = dom->getTopics();

			for (unsigned int t = 0; t < topics.size(); t++) {
				std::string topName = opsHelper.fullTopicName(domainName, topics[t]->getName());
				bool found = false;
				for (unsigned int j = 0; j < args.topicNames.size(); j++) {
					if (args.topicNames[j] == topName) {
						found = true;
						break;
					}
				}
				if (!found) args.topicNames.push_back(topName);
			}
		}

		std::cout << std::endl;

		// Create subscribers for all existing topics
		for (unsigned int i = 0; i < args.topicNames.size(); i++) {
			std::string topName = args.topicNames[i];
			if (topName == "") continue;

			ops::Participant* part = opsHelper.getDomainParticipant(opsHelper.domainName(topName));
			if (!part) continue;

			Topic topic = part->createTopic(opsHelper.topicName(topName));

			std::cout <<
				"Subscribing to Topic: " << opsHelper.fullTopicName(opsHelper.domainName(topName), opsHelper.topicName(topName)) <<
				" [ " << topic.getTransport() <<
				"::" << topic.getDomainAddress() <<
				"::" <<topic.getPort() <<
				" ] " <<
				std::endl;

			ops::Subscriber* sub;
			sub = new ops::Subscriber(topic);
			sub->addDataListener(this);
			sub->start();

			vSubs.push_back(sub);
		}

		// Create subscribers to all ParticipantInfoData
		for (unsigned int i = 0; i < args.infoDomains.size(); i++) {
			try {
				std::string domainName = args.infoDomains[i];
				if (domainName == "") continue;

				ops::Participant* part = opsHelper.getDomainParticipant(domainName);
				if (part == NULL) continue;

				Topic top = part->createParticipantInfoTopic();
				if (top.getPort() == 0) continue;

				std::cout <<
					"Subscribing to Topic: " << opsHelper.fullTopicName(domainName, top.getName()) <<
					" [ " << top.getTransport() <<
					"::" << top.getDomainAddress() <<
					"::" <<top.getPort() <<
					" ] " <<
					std::endl;

				ops::Subscriber* sub = new ops::Subscriber(top);
				sub->addDataListener(this);
				sub->start();

				vSubs.push_back(sub);
			}
			catch(...)
			{
			}
		}

		std::cout << std::endl;

		if (vSubs.size() == 0) {
			std::cout << "##### No subscriptions !!!!. Check topics !!!" << std::endl;
			exit(-1);
		}
	}
	//
	virtual ~Main()
	{
		for (unsigned int i = 0; i < vSubs.size(); i++) {
			vSubs[i]->stop();
			delete vSubs[i];
		}
	}
	//
	///Override from ops::DataListener, called whenever new data arrives.
	void onNewData(ops::DataNotifier* subscriber)
	{
		ops::Subscriber* sub = dynamic_cast<ops::Subscriber*>(subscriber);
		if (sub) {
			ops::OPSMessage* mess = sub->getMessage();
			if (mess == NULL) return;

			// Reserve message and queue, so we don't delay the subscriber thread
			mess->reserve();

			TEntry ent;
			ent.time = sds::sdsSystemTime();
			ent.mess = mess;
			//sub->getTopic().getDomainID();	 could print domain::topic in case topics have the same name in different domains

			ListLock.lock();

			List.push_back(ent);
			messDataCounter++;

			ListLock.unlock();
		}
	}
	//
	void WorkOnList(int numMess)
	{
		/// Don't loop to much, to not loose mmi responsiveness
		for (int loopCnt=0; loopCnt < numMess; loopCnt++) {
			TEntry ent;
			ops::OPSMessage* mess = NULL;
			ops::OPSObject* opsData = NULL;
			ops::ParticipantInfoData* piData = NULL;

			ListLock.lock();
			if (List.size() > 0) {
				ent = List.front();
				mess = ent.mess;
				List.pop_front();
			}
			ListLock.unlock();

			if (mess == NULL) return;

			opsData = mess->getData();

			/// OPSMessage
			//	  int64_t getPublicationID()
			//    std::string getPublisherName()
			//	  std::string mess->getTopicName();
			//    mess->getSource(srcIP, srcPort);
			/// OPSObject
			//    std::string getKey();
			//    const std::string& getTypeString();
			//    std::vector<char> spareBytes;

			piData = dynamic_cast<ops::ParticipantInfoData*>(opsData);

			if (piData == NULL) {
				// Ordinary Topic
				std::string str = "";
				if (logTime) {
					str += "[" + sds::sdsSystemTimeToLocalTime(ent.time) + "] ";
				}
				for(unsigned int i = 0; i < printFormat.size(); i++) {
					str += formatMap[printFormat[i]](mess, opsData) + ", ";
				}
				if (str != "") {
					std::cout << str << std::endl;
				}
			} else {
				// Show Participant Info
				// First check if it's from any of our own participants
				bool fromMe = false;
				for (unsigned int i = 0; i < ownPartInfoNames.size(); i++) {
					if (ownPartInfoNames[i] == piData->name) fromMe = true;
				}
				if (!fromMe) {
					if (partMap.find(piData->name) == partMap.end()) {
						std::cout << "[" << partMap.size() + 1 << "] >>>>> Participant '" << piData->name << "' arrived" << std::endl;
					}
					partMap[piData->name] = sds::sdsSystemTime();
				}
				if (!fromMe && !onlyArrivingLeaving) {
					std::cout <<
						"[" << partMap.size() << "] " <<
						"name: " << piData->name <<
						", domain: " << piData->domain <<
						", partId: " << piData->id <<
						", ip: " << piData->ip <<
						", mcudp: " << piData->mc_udp_port <<
						", mctcp: " << piData->mc_tcp_port <<
						std::endl;
					std::string srcIP;
					int srcPort;
					mess->getSource(srcIP, srcPort);
					std::cout <<
						"  lang: " << piData->languageImplementation <<
						", opsver: " << piData->opsVersion <<
						", From: " << srcIP << ":" << srcPort <<
						", pubId: " << mess->getPublicationID() <<
						std::endl;
					//std::vector<TopicInfoData> subscribeTopics;
					std::cout << "  subscr Topics: ";
					for (unsigned int i = 0; i < piData->subscribeTopics.size(); i++) {
						std::cout << piData->subscribeTopics[i].name << " ";
					}
					std::cout << std::endl;
					//std::vector<TopicInfoData> publishTopics;
					std::cout << "  pub Topics: ";
					for (unsigned int i = 0; i < piData->publishTopics.size(); i++) {
						std::cout << piData->publishTopics[i].name << " ";
					}
					std::cout << std::endl;
					//std::vector<std::string> knownTypes;
					std::cout << "  knownTypes: ";
					for (unsigned int i = 0; i < piData->knownTypes.size(); i++) {
						std::cout << piData->knownTypes[i] << " ";
					}
					std::cout << std::endl;
				}
			}
			mess->unreserve();
		}
	}

	void periodicalThings()
	{
		// Check for disappering participants
		int64_t limit = sds::sdsSystemTime() - sds::msToSdsSystemTimeUnits(5000);
		for (std::map<std::string, int64_t>::iterator it = partMap.begin(); it != partMap.end(); ++it) {
			if (it->second < limit) {
				std::cout << "[" << partMap.size() - 1 << "] <<<<< Participant '" << it->first << "' has left" << std::endl;
				partMap.erase(it);
				break;
			}
		}
	}
};

#ifndef _WIN32
#include <time.h>
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
#endif

int main(int argc, char* argv[])
{
	std::cout << std::endl << "OPSListener Version 2016-12-10" << std::endl << std::endl;

	sds::sdsSystemTimeInit();

	CArguments args;

	if (!args.HandleCommandLine(argc, argv)) goto doShowUsage;
	if (!args.ValidateArguments()) goto doShowUsage;

	{
		//Create an object that will listen to OPS events
		Main* m = new Main(args);

		bool doPause = false;
		int numMess = 100;

		while (true) {
			if (_kbhit()) {
				char buffer[1024];
				if (fgets(buffer, sizeof(buffer), stdin) != NULL) {
					std::string line(buffer);

					// trim start
					std::string::size_type idx = line.find_first_not_of(" \t");
					if (idx == std::string::npos) continue;
					if (idx > 0) line.erase(0, idx);
					if (line.size() == 0) continue;

					char ch = line[0];
					line.erase(0, 1);

					if (ch == 0x1b) break;
					if ((ch == 'q') || (ch == 'Q')) break;
					if ((ch == 'x') || (ch == 'X')) break;

					if ((ch == 'p') || (ch == 'P')) {
						doPause = !doPause;
					}
					if ((ch == 's') || (ch == 'S')) {
						doPause = true;
						m->WorkOnList(1);
					}

					//if (ch == '?') {
					//	std::cout << "Subscribing to topic: " << topicNames[0];
					//	std::cout << std::endl;
					//}
				}
			}
			ops::TimeHelper::sleep(10);

			if (doPause) {
				printf("Queued %d\r", m->numQueued());
			} else {
				m->WorkOnList(numMess);
			}
			m->periodicalThings();
		}

		delete m;
	}
	return 0;

doShowUsage:
	ShowUsage();
	return 0;
}
