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
	appendType(TypeId_T("Domain"));
}

Address_T Domain::getDomainAddress()
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

Topic Domain::getTopic(ObjectName_T name)
{
	for(unsigned int i = 0 ; i < topics.size(); i++)
	{
		if(topics[i]->getName() == name) 
		{
			checkTopicValues(topics[i]);
			return *topics[i];
		}
	}
	ExceptionMessage_T msg = "Topic ";
	msg += name;
	msg += " does not exist in ops config file.";
	throw NoSuchTopicException(msg);
}

bool Domain::existsTopic(ObjectName_T name)
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

ObjectName_T Domain::getDomainID()
{
	return domainID;
}

void Domain::serialize(ArchiverInOut* archiver)
{
	OPSObject::serialize(archiver);
	archiver->inout("domainID", domainID);
	archiver->inout<Topic>("topics", topics);
	archiver->inout("domainAddress", domainAddress);
	archiver->inout("localInterface", localInterface);
	archiver->inout("timeToLive", timeToLive);
	archiver->inout("inSocketBufferSize", inSocketBufferSize);
	archiver->inout("outSocketBufferSize", outSocketBufferSize);
	archiver->inout("metaDataMcPort", metaDataMcPort);

	// To not break binary compatibility we only do this when we know we are
	// reading from an XML-file
	if (dynamic_cast<XMLArchiverIn*>(archiver) != NULL) { 
		archiver->inout<Channel>("channels", channels);
		archiver->inout<Transport>("transports", transports);
		checkTransports();
	}
}

Channel* Domain::findChannel(ChannelId_T id)
{
	if (id != "") {
		for (unsigned int i = 0; i < channels.size(); i++) {
			if (id == channels[i]->channelID) return channels[i];
		}
	}
	return NULL;
}

Topic* Domain::findTopic(ObjectName_T id)
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
			ExceptionMessage_T msg("Non existing channelID: '");
			msg += transports[i]->channelID;
			msg += "' used in transport specification.";
			throw ops::ConfigException(msg);
		} else {
			for (unsigned int j = 0; j < transports[i]->topics.size(); j++) {
				Topic* top = findTopic(transports[i]->topics[j]);
				if (top == NULL) {
					ExceptionMessage_T msg("Non existing topicID: '");
					msg += transports[i]->topics[j];
					msg += "' used in transport specification.";
					throw ops::ConfigException(msg);
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

Address_T Domain::getLocalInterface()
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
	for (auto it = topics.begin(); it != topics.end(); ++it) {
		delete (*it);
	}
	topics.clear();
	for (auto it = channels.begin(); it != channels.end(); ++it) {
		delete (*it);
	}
	channels.clear();
	for (auto it = transports.begin(); it != transports.end(); ++it) {
		delete (*it);
	}
	transports.clear();
}

}
