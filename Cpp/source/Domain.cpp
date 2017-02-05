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
#ifndef REPLACE_TRANSPORT_LAYER
#include "BoostIOServiceImpl.h"
#endif
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
	if (top->getDomainAddress() == "") top->setDomainAddress(domainAddress);
	if (top->getLocalInterface() == "") top->setLocalInterface(localInterface);
	if (top->getTimeToLive() < 0) top->setTimeToLive(timeToLive);
	if (top->getInSocketBufferSize() < 0) top->setInSocketBufferSize(inSocketBufferSize);
	if (top->getOutSocketBufferSize() < 0) top->setOutSocketBufferSize(outSocketBufferSize);
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

}
