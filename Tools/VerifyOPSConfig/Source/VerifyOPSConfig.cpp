// VerifyOPSConfig.cpp : Defines the entry point for the console application.
//

#include <fstream>
#include <vector>

#include "OPSConfig.h"
#include "PrintArchiverOut.h"

#include "Configuration.h"

const std::string sVersion = "Version 2017-09-10";


bool gWarningGiven = false;

#define LOG_WARN(message) { std::cout << message; gWarningGiven = true; } 

#define LOG_DEBUG(message) { if (bDebug) std::cout << message; } 

class CVerifyOPSConfig 
{
private:
	// Arguments
	bool bDebug;
	
	// 
	int iNumDomains;
	std::vector<std::string> vDomains;
	std::vector<std::string> vDomainMeta;
	std::vector<std::string> vTopics;
	std::vector<std::string> vChannels;
	std::vector<std::string> vTransportTopics;
	bool udpTransportUsed;

public:
	CVerifyOPSConfig(std::string filename, bool debug):
	    bDebug(debug),
		iNumDomains(0),
		udpTransportUsed(false)
	{
		try {
			std::string s;
			Configuration config(filename);

			LOG_DEBUG( std::endl << "Checking file: " << filename << std::endl << std::endl );

			// Some error control. 
			s = config.getParseResult();
			if (s != "") {
				LOG_WARN( ">>> Error parsing file, Result: " << s << std::endl );
				return;
			}

			//<root>
			//    <ops_config type = "DefaultOPSConfigImpl">
			//        <domains>

			config.root();
			if (!config.enter("root")) {
			LOG_WARN( ">>> Missing <root>" << std::endl );
				return;
			}
			if (!config.enter("ops_config")) {
				LOG_WARN( ">>> Missing <ops_config ...>" << std::endl );
				return;
			}
			if (config.getAttribute("type") != "DefaultOPSConfigImpl") {
				LOG_WARN( ">>> Unknown <ops_config type=" << config.getAttribute("type") << "> " << std::endl );
			}

			if (config.numEntries("domains") > 1) {
				LOG_WARN( ">>> File should only contain ONE <domains> section" << std::endl );
			}

			if (!config.enter("domains")) {
				LOG_WARN( ">>> Missing <domains>" << std::endl );
				return;
			}

			for (int iDomains = 0; iDomains < config.numEntries("element"); iDomains++) {
				if (config.enter("element", iDomains)) {
					iNumDomains++;
					VerifyDomain(config);
					config.exit();
				}
			}
		}
		catch (...)
		{
		}
		LOG_DEBUG("Total # domains: " << iNumDomains << std::endl);

		// We have checked the xml-file.
		// Now do further checks using the ops objects created from the xml-file
		ops::OPSConfig* cfg = ops::OPSConfig::getConfig(filename.c_str());
		if (cfg) {
			// Trick to get all topics fully initialized
			std::vector<ops::Domain*> domains = cfg->getRefToDomains();
			for (unsigned int i = 0; i < domains.size(); i++) {
				domains[i]->getTopics();
			}

			// 


			if (bDebug) {
				// Dump all domains and topics to standard out 
				ops::PrintArchiverOut prt(std::cout);
				prt.printObject("", cfg);
				std::cout << std::endl;
			}
		}
	}

	bool CheckExist(std::string name, std::vector<std::string>& vect)
	{
		for (unsigned int i = 0; i < vect.size(); i++) {
			if (vect[i] == name) return true;
		}
		return false;
	}

	bool CheckDuplicate(std::string name, std::vector<std::string>& vect)
	{
		for (unsigned int i = 0; i < vect.size(); i++) {
			if (vect[i] == name) return true;
		}
		vect.push_back(name);
		return false;
	}

	bool EraseIfExist(std::string name, std::vector<std::string>& vect)
	{
		for (unsigned int i = 0; i < vect.size(); i++) {
			if (vect[i] == name) {
				vect.erase(vect.begin() + i);
				return true;
			}
		}
		return false;
	}

	void VerifyDomain(Configuration& config)
	{
		std::string domainName, s;
		int iNumTopics = 0;
		vChannels.clear();
		vTransportTopics.clear();
		udpTransportUsed = false;

		//<element type = "MulticastDomain"> or <element type = "Domain">
        //    <domainID>SDSDomain</domainID>
        //    <domainAddress>239.7.1.6</domainAddress>
 	    //    <localInterface>127.0.0.1</localInterface>
        //    <topics>
        //        <element type = "Topic">
        //            <name>RecordingControlTopic</name>
        //            <port>7001</port>
        //            <dataType>sds.RecordingControlData</dataType>
        //        </element>

		std::string domainType = config.getAttribute("type");
		if ((domainType != "MulticastDomain") && (domainType != "Domain")) {
			LOG_WARN( ">>> Unknown <element type=" << domainType << "> " << std::endl );
		}

		// domainID
		domainName = config.getString("domainID");
		if (domainName == "") {
			LOG_WARN( ">>> Missing <domainID> for a domain" << std::endl );
			domainName = "Missing <domainID>";
		} else {
			LOG_DEBUG( "Domain: " << domainName << std::endl );
			// Check unique domain name
			if (CheckDuplicate(domainName, vDomains)) {
				LOG_WARN( ">>> Duplicate domain detected. '" << domainName << "' already used." << std::endl );
			}
		}

		// domainAddress
		std::string domainAddress = config.getString("domainAddress");
		if (domainAddress == "") {
			LOG_WARN( ">>> Missing <domainAddress> for domain: " << domainName << std::endl );
		}

		// metaDataMcPort, optional but necessary if UDP transports are used.
		// If not set to 0, then DomainAddress::metaDataMcPort should be unique
		std::string metaDataMcPort = config.getString("metaDataMcPort");
		LOG_DEBUG("metaDataMcPort: '" << metaDataMcPort << "', parsed as: " << config.parseInt(metaDataMcPort, 9494) << std::endl );
		if (config.parseInt(metaDataMcPort, 9494) != 0) {
			if (CheckDuplicate(domainAddress + "::" + metaDataMcPort, vDomainMeta)) {
				LOG_WARN(">>> Duplicate use of DomainAddress::metaDataMcPort for domain " << domainName << std::endl);
			}
		}

		// localInterface, optional

		// channels, optional
		if (config.numEntries("channels") > 1) {
			LOG_WARN( ">>> Domain should only contain at most ONE <channels> section" << std::endl );
		}
		if (config.enter("channels")) {
			for (int iChannels = 0; iChannels < config.numEntries("element"); iChannels++) {
				if (config.enter("element", iChannels)) {
					VerifyChannel(config, domainName);
					config.exit();
				}
			}
			config.exit();
		}

		// transports, optional
		if (config.numEntries("transports") > 1) {
			LOG_WARN( ">>> Domain should only contain at most ONE <transports> section" << std::endl );
		}
		if (config.enter("transports")) {
			for (int iTransports = 0; iTransports < config.numEntries("element"); iTransports++) {
				if (config.enter("element", iTransports)) {
					VerifyTransport(config, domainName);
					config.exit();
				}
			}
			config.exit();
		}

		// topics
		if (config.numEntries("topics") > 1) {
			LOG_WARN( ">>> Domain should only contain ONE <topics> section" << std::endl );
		}
		if (!config.enter("topics")) {
			LOG_WARN( ">>> Missing <topics> for domain: " << domainName << std::endl );

		} else {
			vTopics.clear();
			for (int iTopics = 0; iTopics < config.numEntries("element"); iTopics++) {
				if (config.enter("element", iTopics)) {
					iNumTopics++;
					VerifyTopic(config, domainName);
					config.exit();
				}
			}
			config.exit();
		}

		// Check if udp is used which require a metaDataMcPort
		if (udpTransportUsed && (config.parseInt(metaDataMcPort, 9494) == 0)) {
			LOG_WARN( ">>> UDP used as transport and '<metaDataMcPort>' set to 0. UDP requires metaDataMcPort != 0." << std::endl );
		}

		// Check that all transport topics have been found
		if (vTransportTopics.size() > 0) {
			for (unsigned int i = 0; i < vTransportTopics.size(); i++) {
				LOG_WARN( ">>> Missing topic definition in domain: " << domainName << " for topic: " << vTransportTopics[i] << std::endl );
			}
		}

		LOG_DEBUG( " # Topics: " << iNumTopics << std::endl );
	}

	void VerifyChannel(Configuration& config, std::string domainName)
	{
		std::string s;

		//must have a unique channel name
		std::string channelName = config.getString("name");
		if (channelName == "") {
			LOG_WARN( ">>> Missing <name> for a channel" << std::endl );
			channelName = "Missing <name>";
		} else {
			LOG_DEBUG( "Channel: " << channelName << std::endl );
			// Check unique channel name
			if (CheckDuplicate(channelName, vChannels)) {
				LOG_WARN( ">>> Duplicate channel name detected in domain '" << domainName << "'. Channel name '" << channelName << "' already used." << std::endl );
			}
		}

		// linktype
		std::string linkType = config.getString("linktype");
		if (linkType == "") {
			LOG_WARN( ">>> Missing <linktype> for channel '" << channelName << "' in domain '" << domainName << "'" << std::endl );
		}
		if (linkType == "udp") udpTransportUsed = true;

		// address
		s = config.getString("address");
		if ((s == "") && (linkType != "udp")) {
			LOG_WARN( ">>> Missing <address> for channel '" << channelName << "' in domain '" << domainName << "'" << std::endl );
		}

		// port
		s = config.getString("port");
		if (s == "") {
			LOG_WARN( ">>> Missing <port> for channel '" << channelName << "' in domain '" << domainName << "'" << std::endl );
		} else {
			if (config.parseInt(s, -1) > 65535) {
				LOG_WARN(">>> port > 65535 for channel '" << channelName << "'" << std::endl);
			}
		}

		///TODO can have local interface, buffer sizes
	}

	void VerifyTransport(Configuration& config, std::string domainName)
	{
		///must have an existing channel name
		std::string channelName = config.getString("channelID");
		if (channelName == "") {
			LOG_WARN( ">>> Missing <channelID> for a transport" << std::endl );
			channelName = "Missing <channelID>";
		} else {
			LOG_DEBUG( "  channelID: " << channelName << std::endl );
			if (!CheckExist(channelName, vChannels)) {
				LOG_WARN( ">>> Unknown channel name '" << channelName << "' used in transport for domain '" << domainName << "'." << std::endl );
			}
		}

		///can have zero or more topic names (save all topicnames and check that they are defined in topics section)
		///check that topic only exist in one transport
		if (config.numEntries("topics") > 1) {
			LOG_WARN( ">>> Transports should only contain ONE <topics> section" << std::endl );
		}
		if (!config.enter("topics")) {
			LOG_WARN( ">>> Missing <topics> for transport '" << channelName << "' in domain '" << domainName << "'" << std::endl );

		} else {
			for (int iTopics = 0; iTopics < config.numEntries("element"); iTopics++) {
				std::string topicName = config.getString("element", iTopics);
				LOG_DEBUG( "    topicName: " << topicName << std::endl );
				if (CheckDuplicate(topicName, vTransportTopics)) {
					LOG_WARN( ">>> Duplicate topic detected in transport for domain '" << domainName << "'. Topic name '" << topicName << "' already specified." << std::endl );
				}
			}
			config.exit();
		}
	}

	void VerifyTopic(Configuration& config, std::string domainName)
	{
		std::string topicName, dataType, s;
		uint64_t sampleMaxSize;

		//<element type = "Topic">
		//  <name>PingTopic</name>
	    //  <port>8003</port>
		//  <dataType>sds.TemplateMessageData_struct_sPing</dataType>
		//  <sampleMaxSize>10000000</sampleMaxSize>
		//  <inSocketBufferSize>32000000</inSocketBufferSize>
		//  <outSocketBufferSize>32000000</outSocketBufferSize>
		//</element>

		if (config.getAttribute("type") != "Topic") {
			LOG_WARN( ">>> Unknown <element type=...> " << std::endl );
		}

		// name
		topicName = config.getString("name");
		if (topicName == "") {
			LOG_WARN( ">>> Missing <name> for a topic" << std::endl );
			topicName = "Missing <name>";
		} else {
			LOG_DEBUG( "Checking topic: " << topicName << std::endl );
			// Check that name is unique in domain
			if (CheckDuplicate(topicName, vTopics)) {
				LOG_WARN( ">>> Duplicate Topic detected. Name '" << topicName << "' already used." << std::endl );
			}
		}

		//
		bool topicDefinedInTransports = EraseIfExist(topicName, vTransportTopics);

		// port
		s = config.getString("port");
		if (s == "") {
			if (!topicDefinedInTransports) {
				LOG_WARN( ">>> Missing <port> for topic: " << topicName << ", in domain: " << domainName << std::endl );
			}
		} else {
			if (topicDefinedInTransports) {
				LOG_WARN( ">>> <port> specified for topic: " << topicName << ", in domain: " << domainName << ". Transport/Channel values will override." << std::endl );
			} else {
				if (config.parseInt(s, -1) > 65535) {
					LOG_WARN(">>> port > 65535 for topic '" << topicName << "'" << std::endl);
				}
			}
		}

		// dataType
		dataType = config.getString("dataType");
		if (dataType == "") {
			LOG_WARN( ">>> Missing <dataType> for topic: " << topicName << ", in domain: " << domainName << std::endl );
		}

		// sampleMaxSize optional, required if sample size os > 60000
		sampleMaxSize = config.parseInt64(config.getString("sampleMaxSize"), 60000);
		if (sampleMaxSize > 60000) {
			// For datatypes > 60000, we require different ports
			///TODO
		}

		// transport optional
		std::string linkType = config.getString("transport");
		if (linkType == "udp") udpTransportUsed = true;

		// inSocketBufferSize optional
		// outSocketBufferSize optional
	}

};

void PrintDescription()
{
	std::cout << std::endl;
	std::cout << "  " << sVersion << std::endl;
	std::cout << "  " << std::endl;
	std::cout << "  Program verifies that: " << std::endl;
	std::cout << "    - XML syntax is OK for OPS usage" << std::endl;
	std::cout << "    - OPS required entries exist" << std::endl;
	std::cout << "    - Domain names are unique" << std::endl;
	std::cout << "    - Channel names are unique within the given domain" << std::endl;
	std::cout << "    - Topic names are unique within the given domain" << std::endl;
	std::cout << "  " << std::endl;
	std::cout << std::endl;
}

void Usage()
{
	std::cout << std::endl;
	std::cout << "  Usage:" << std::endl;
	std::cout << "    VerifyOPSConfig [-?] [-debug] filename " << std::endl;
	std::cout << std::endl;
	std::cout << "    -?          Help" << std::endl;
	std::cout << "    -debug      Print some debug info during work" << std::endl;
}

int main(int argc, char* argv[])
{
	std::string infile = "";
	bool printUsage = false;
	bool printDescription = false;
	bool debug = false;

	for (int i = 1; i < argc; i++) {
		std::string arg = argv[i];
		if (arg == "?") {
			printUsage = true;
			printDescription = true;

		} else if (arg == "-?") {
			printUsage = true;
			printDescription = true;

		} else if (arg == "-debug") {
			debug = true;

		} else {
			infile = arg;
			break;
		}
	}

	if (printUsage || (infile == "")) {
		Usage();
		if (printDescription) {
			PrintDescription();
		}

	} else {
		std::cout << std::endl;

		CVerifyOPSConfig verify(infile, debug);

		if (!gWarningGiven) std::cout << "Check OK  " << std::endl;

		std::cout << std::endl;
	}

	return 0;
}

