#ifndef __COpsConfigHelper_h__
#define __COpsConfigHelper_h__

#include <vector>
#include <map>

#include "ops.h"

class ILogInterface
{
public:
	virtual void Log(const char* const szFormatString, ...) = 0;
};

class COpsConfigHelper
{
private:
	ILogInterface* pInfoLog;
	ILogInterface* pErrorLog;
	ops::ObjectName_T sDefaultDomain;					// Default domain if not specified for a topic

public:
	COpsConfigHelper( ILogInterface* infoLogHandler,
		ILogInterface* errorLogHandler,
		ops::ObjectName_T defaultDomain) :
		pInfoLog(infoLogHandler),
		pErrorLog(errorLogHandler),
		sDefaultDomain(defaultDomain)
		{}

	// -----------------------------------------------------------------------------------------
	// Map with Domain --> ops configuration file
	std::map<ops::ObjectName_T, ops::FileName_T> domainMap;

	bool existsDomain(ops::ObjectName_T domain)
	{
		return (domainMap.find(domain) != domainMap.end());
	}

	bool DomainMapAdd(ops::ObjectName_T domain, ops::FileName_T cfgFile)
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

	void DomainMapAdd(ops::FileName_T cfgFile)
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

	void getAvailableDomains(std::vector<ops::ObjectName_T>& domains)
	{
		domains.clear();
		for (std::map<ops::ObjectName_T, ops::FileName_T>::iterator it = domainMap.begin(); it != domainMap.end(); ++it) {
			domains.push_back(it->first);
		}
	}

	ops::Participant* getDomainParticipant(ops::ObjectName_T name)
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
	std::vector<ops::ObjectName_T> vDomains;

	void checkTopicDomain(ops::ObjectName_T topicName)
	{
		ops::ObjectName_T s = ops::utilities::domainName(topicName);
		for (unsigned int i = 0; i < vDomains.size(); i++) {
			if (s == vDomains[i]) return;
		}
		vDomains.push_back(s);
	}

	// -----------------------------------------------------------------------------------------
	// Check if topic name exists in the domain
	bool existsTopic(ops::ObjectName_T name)
	{
		bool exist = false;
		try {
			ops::Participant* part = getDomainParticipant(ops::utilities::domainName(name));
			if (part) {
				ops::Domain* domain = part->getDomain();
				if (domain) exist = domain->existsTopic(ops::utilities::topicName(name));
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
	bool verifyTopicType(ops::Topic& top, ops::TypeId_T typeName)
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
