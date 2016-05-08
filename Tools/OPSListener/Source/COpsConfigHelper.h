#ifndef __COpsConfigHelper_h__
#define __COpsConfigHelper_h__

#include <vector>
#include <map>

#include <ops.h>

class ILogInterface
{
public:
	virtual void Log(const char* szFormatString, ...) = 0;
};

class COpsConfigHelper
{
private:
	ILogInterface* pInfoLog;
	ILogInterface* pErrorLog;
	std::string sDefaultDomain;					// Default domain if not specified for a topic

public:
	COpsConfigHelper( ILogInterface* infoLogHandler,
		ILogInterface* errorLogHandler,
		std::string defaultDomain) :
		pInfoLog(infoLogHandler),
		pErrorLog(errorLogHandler),
		sDefaultDomain(defaultDomain)
		{}

	// -----------------------------------------------------------------------------------------
	// Map with Domain --> ops configuration file
	std::map<std::string, std::string> domainMap;

	bool existsDomain(std::string domain)
	{
		return (domainMap.find(domain) != domainMap.end());
	}

	bool DomainMapAdd(std::string domain, std::string cfgFile)
	{
		if (domainMap.find(domain) == domainMap.end()) {
			domainMap[domain] = cfgFile;
			if (pInfoLog) pInfoLog->Log("Using domain '%s' from file: %s\n", domain.c_str(), cfgFile.c_str());
			return true;
		} else {
			if (pErrorLog) pErrorLog->Log("##### Error: Domain '%s' specified more than once. Using first instance.\n", domain.c_str());
			return false;
		}
	}

	void DomainMapAdd(std::string cfgFile)
	{
		ops::OPSConfig* cfg = NULL;
		try {
			cfg = ops::OPSConfig::getConfig(cfgFile);
			if (!cfg) {
				if (pErrorLog) pErrorLog->Log("##### Error: Failed to handle configuration file '%s'\n", cfgFile.c_str());
				return;
			}
			std::vector<ops::Domain*> domains = cfg->getDomains();
			for (unsigned int i = 0; i < domains.size(); i++) {
				DomainMapAdd(domains[i]->getDomainID(), cfgFile);
			}
		}
		catch (...) {
			if (pErrorLog) pErrorLog->Log("##### Error: Failed to read/use configuration file '%s'\n", cfgFile.c_str());
		}
		if (cfg) delete cfg;
	}

	ops::Participant* getDomainParticipant(std::string name)
	{
		if (domainMap.find(name) == domainMap.end()) {
			// Not found so, use default
			if (pInfoLog) pInfoLog->Log("Using default 'ops_config.xml' file for domain: %s\n", name.c_str());
			return ops::Participant::getInstance(name, name);
		} else {
			return ops::Participant::getInstance(name, name, domainMap[name]);
		}
	}

	// -----------------------------------------------------------------------------------------
	// Domains specified with topics (added with method below)
	std::vector<std::string> vDomains;

	void checkTopicDomain(std::string topicName)
	{
		std::string s = domainName(topicName);
		for (unsigned int i = 0; i < vDomains.size(); i++) {
			if (s == vDomains[i]) return;
		}
		vDomains.push_back(s);
	}

	// -----------------------------------------------------------------------------------------
	// Parses a topic name that includes domain name using syntax 'Domain::TopicName'
	//
	static std::string topicName(std::string name)
	{
		std::basic_string <char>::size_type index1;
		std::string s = name;
		if ((index1 = s.find("::")) != std::string::npos) {
			s.erase(0, index1+2);
		}
		return s;
	}

	// If no domain given, the default domain is returned
	std::string domainName(std::string name)
	{
		std::basic_string <char>::size_type index1;
		std::string s = name;
		if ((index1 = s.find("::")) != std::string::npos) {
			s.erase(index1, std::string::npos);
			if (s != "") return s;
		}
		return sDefaultDomain;
	}

	// Builds a full topicname on the format 'Domain::TopicName'
	static std::string fullTopicName(std::string domainName, std::string topicName)
	{
		return domainName + "::" + topicName;
	}

	// -----------------------------------------------------------------------------------------
	// Check if topic name exists in the domain
	bool existsTopic(std::string name)
	{
		bool exist = false;
		try {
			ops::Participant* part = getDomainParticipant(domainName(name));
			if (part) {
				ops::Domain* domain = part->getDomain();
				if (domain) exist = domain->existsTopic(topicName(name));
			}
		}
		catch (...) {
			exist = false;
		}
		if (!exist) {
			if (pErrorLog) pErrorLog->Log("##### Topic: '%s' doesn't exist\n", name.c_str());
		}
		return exist;
	}

	// -----------------------------------------------------------------------------------------
	//
	bool verifyTopicType(ops::Topic& top, std::string typeName)
	{
		bool expected = (top.getTypeID() == typeName);
		if (!expected) {
			if (pErrorLog) pErrorLog->Log("##### Error: Topic '%s' of wrong type '%s' (expected '%s')",
								top.getName().c_str(), top.getTypeID().c_str(), typeName.c_str());
		}
		return expected;
	}
};

#endif  // __COpsConfigHelper_h__
