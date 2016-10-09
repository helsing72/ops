/**
* 
* Copyright (C) 2006-2009 Anton Gravestam.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.

* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*/
#include "OPSTypeDefs.h"
#include "Domain.h"
#include "NoSuchTopicException.h"
#include "BoostIOServiceImpl.h"
#include "XMLArchiverIn.h"
#include "ConfigException.h"

namespace ops
{

Domain::Domain() : 
	timeToLive(1), 
	localInterface("0.0.0.0"),
	inSocketBufferSize(-1),		// Use OS default, Topics may override
	outSocketBufferSize(-1),	// Use OS default, Topics may override
	metaDataMcPort(9494)		// Default port 
{
	appendType(std::string("Domain"));
}

std::string Domain::getDomainAddress()
{
	return domainAddress;
}

void Domain::checkTopicValues(Topic* top)
{
	if (top->getDomainAddress() == "") 
	{
		top->setDomainAddress(domainAddress);
	}
	if (top->getLocalInterface() == "") 
	{
		top->setLocalInterface(localInterface);
	}
	if (top->getInSocketBufferSize() < 0) 
	{
		top->setInSocketBufferSize(inSocketBufferSize);
	}
	if (top->getOutSocketBufferSize() < 0) 
	{
		top->setOutSocketBufferSize(outSocketBufferSize);
	}
}

std::vector<Topic* > Domain::getTopics()
{
	for(unsigned int i = 0 ; i < topics.size(); i++) 
	{
		checkTopicValues(topics[i]);
	}
	return topics;
}

Topic Domain::getTopic(std::string name)
{
	for(unsigned int i = 0 ; i < topics.size(); i++)
	{
		if(topics[i]->getName() == name) 
		{
			checkTopicValues(topics[i]);
			return *topics[i];
		}
	}
	throw NoSuchTopicException("Topic " + name + " does not exist in ops config file.");
}

bool Domain::existsTopic(std::string name)
{
	for(unsigned int i = 0 ; i < topics.size(); i++)
	{
		if(topics[i]->getName() == name)
		{
			return true;
		}
	}
	return false;
}

std::string Domain::getDomainID()
{
	return domainID;
}

void Domain::serialize(ArchiverInOut* archiver)
{
	OPSObject::serialize(archiver);
	archiver->inout(std::string("domainID"), domainID);
	archiver->inout<Topic>(std::string("topics"), topics);
	archiver->inout(std::string("domainAddress"), domainAddress);
	archiver->inout(std::string("localInterface"), localInterface);
	archiver->inout(std::string("timeToLive"), timeToLive);
	archiver->inout(std::string("inSocketBufferSize"), inSocketBufferSize);
	archiver->inout(std::string("outSocketBufferSize"), outSocketBufferSize);
	archiver->inout(std::string("metaDataMcPort"), metaDataMcPort);

	// To not break binary compatibility we only do this when we know we are
	// reading from an XML-file
	if (dynamic_cast<XMLArchiverIn*>(archiver) != NULL) { 
		archiver->inout<Channel>(std::string("channels"), channels);
		archiver->inout<Transport>(std::string("transports"), transports);
		checkTransports();
	}
}

Channel* Domain::findChannel(std::string id)
{
	if (id != "") {
		for (unsigned int i = 0; i < channels.size(); i++) {
			if (id == channels[i]->channelID) return channels[i];
		}
	}
	return NULL;
}

Topic* Domain::findTopic(std::string id)
{
	if (id != "") {
		for (unsigned int i = 0; i < topics.size(); i++) {
			if (id == topics[i]->getName()) return topics[i];
		}
	}
	return NULL;
}

void Domain::checkTransports()
{
	// Now update topics with values from the transports and channels
	// Loop over all transports and for each topic, see if it needs parameters from the channel
	for (unsigned int i = 0; i < transports.size(); i++) {
		// Get channel
		Channel* channel = findChannel(transports[i]->channelID);
		if (channel == NULL) {
			throw ops::ConfigException(
				std::string("Non existing channelID: '") + transports[i]->channelID +
				std::string("' used in transport spcification."));
		} else {
			for (unsigned int j = 0; j < transports[i]->topics.size(); j++) {
				Topic* top = findTopic(transports[i]->topics[j]);
				if (top == NULL) {
					throw ops::ConfigException(
						std::string("Non existing topicID: '") + transports[i]->topics[j] +
						std::string("' used in transport spcification."));
				} else {
					channel->populateTopic(top);
				}
			}
		}
	}
}

int Domain::getTimeToLive()
{
	return timeToLive;
}

std::string Domain::getLocalInterface()
{
	return localInterface;
}

int Domain::getInSocketBufferSize()
{
	return inSocketBufferSize;
}

int Domain::getOutSocketBufferSize()
{
	return outSocketBufferSize;
}

int Domain::getMetaDataMcPort()
{
	return metaDataMcPort;
}

Domain::~Domain()
{
}

// If argument contains a "/" we assume it is on the form:  subnet-address/subnet-mask
// e.g "192.168.10.0/255.255.255.0" or "192.168.10.0/24"
// In that case we loop over all interfaces and take the first one that matches
// i.e. the one whos interface address is on the subnet
std::string Domain::doSubnetTranslation(std::string addr, IOService* ioServ)
{
	using boost::asio::ip::udp;

	std::basic_string <char>::size_type index;

	index = addr.find("/");
	if (index == std::string::npos) return addr;

	std::string subnet = addr.substr(0, index);
	std::string mask = addr.substr(index+1);

	unsigned long subnetIp = boost::asio::ip::address_v4::from_string(subnet).to_ulong();
	unsigned long subnetMask;
	if (mask.length() <= 2) {
		// Expand to the number of bits given
		subnetMask = atoi(mask.c_str());
		subnetMask = (((1 << subnetMask)-1) << (32 - subnetMask)) & 0xFFFFFFFF;
	} else {
		subnetMask = boost::asio::ip::address_v4::from_string(mask).to_ulong();
	}

	boost::asio::io_service* ioService = ((BoostIOServiceImpl*) ioServ)->boostIOService;

	// Note: The resolver requires that the hostname can be used to resolve to an ip
	// e.g due to the hostname beeing listed with an ipv4 address in /etc/hosts.
	// On linux this can be tested by using the command "hostname -i"
	udp::resolver resolver(*ioService);
	udp::resolver::query query(boost::asio::ip::host_name(), "");
	udp::resolver::iterator it = resolver.resolve(query);
	udp::resolver::iterator end;
	while (it != end) {
		boost::asio::ip::address ipaddr = it->endpoint().address();
		if (ipaddr.is_v4()) {
			unsigned long Ip = ipaddr.to_v4().to_ulong();
			if ((Ip & subnetMask) == subnetIp) 
				return ipaddr.to_string();
		}
		it++;
	}

	return subnet;
}

}

