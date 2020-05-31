/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
 * Copyright (C) 2019-2020 Lennart Andersson.
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

#include "Topic.h"
#include "opsidls/OPSConstants.h"
#include "ConfigException.h"

namespace ops
{
    using namespace opsidls;

	Topic::Topic(ObjectName_T namee, int const portt, TypeId_T typeIDd, Address_T domainAddresss) :
		name(namee), 
		port(portt), 
		typeID(typeIDd), 
		domainAddress(domainAddresss),
		participantID(OPSConstants::DEFAULT_PARTICIPANT_ID()),
		sampleMaxSize(OPSConstants::PACKET_MAX_SIZE)
	{
		appendType(TypeId_T("Topic"));
	}
	Topic::Topic() :
		participantID(OPSConstants::DEFAULT_PARTICIPANT_ID()),
		sampleMaxSize(OPSConstants::PACKET_MAX_SIZE)
	{
		appendType(TypeId_T("Topic"));
	}

	void Topic::setParticipantID(ObjectName_T const partID)
	{
		participantID = partID;
	}

	ObjectName_T Topic::getParticipantID() const
	{
		return participantID;
	}

	void Topic::setDomainID(ObjectName_T const domID)
	{
		domainID = domID;
	}
	ObjectName_T Topic::getDomainID() const
	{
		return domainID;
	}
	ObjectName_T Topic::getName() const
	{
		return name;
	}
	TypeId_T Topic::getTypeID() const
	{
		return typeID;
	}
	void Topic::setDomainAddress(Address_T const domainAddr)
	{
		domainAddress = domainAddr;
	}
	void Topic::setTransport(Transport_T const transp)
	{
		transport = transp;
	}
	Address_T Topic::getDomainAddress() const
	{
		return domainAddress;
	}
	void Topic::setLocalInterface(Address_T const localIf)
	{
		localInterface = localIf;
	}
	Address_T Topic::getLocalInterface() const
	{
		return localInterface;
	}
	int Topic::getSampleMaxSize() const
	{
		return sampleMaxSize;
	}
	void Topic::setSampleMaxSize(int const size)
	{
		if(size < OPSConstants::PACKET_MAX_SIZE)
		{
			sampleMaxSize = OPSConstants::PACKET_MAX_SIZE;
		}
		else
		{
			sampleMaxSize = size;
		}
	}
	int Topic::getPort() const
	{
		return port;
	}
	void Topic::setPort(int const port)
	{
		this->port = port;
	}
	void Topic::setTimeToLive(int const ttl)
	{
		timeToLive = ttl;
	}
	int Topic::getTimeToLive() const
	{
		return timeToLive;
	}
	Transport_T Topic::getTransport() const
	{
		return transport;
	}

	int64_t Topic::getOutSocketBufferSize() const
	{
		return outSocketBufferSize;
	}
	void Topic::setOutSocketBufferSize(int64_t const size)
	{
		outSocketBufferSize = size;
	}
	int64_t Topic::getInSocketBufferSize() const
	{
		return inSocketBufferSize;
	}
	void Topic::setInSocketBufferSize(int64_t const size)
	{
		inSocketBufferSize = size;
	}

	bool Topic::getOptNonVirt() const
	{
		return optNonVirt;
	}

	int Topic::getHeartbeatPeriod() const
	{
		return heartbeatPeriod;
	}
	int Topic::getHeartbeatTimeout() const
	{
		return heartbeatTimeout;
	}

	ChannelId_T Topic::getChannelId() const
	{
		return channelID;
	}

	void Topic::serialize(ArchiverInOut* const archiver)
	{
		OPSObject::serialize(archiver);
		archiver->inout("name", name);
		archiver->inout("dataType", typeID);
		archiver->inout("port", port);		
		archiver->inout("address", domainAddress);

		archiver->inout("outSocketBufferSize", outSocketBufferSize);
		archiver->inout("inSocketBufferSize", inSocketBufferSize);
	

		//Limit this value 
		int tSampleMaxSize = getSampleMaxSize();
		archiver->inout("sampleMaxSize", tSampleMaxSize);
		setSampleMaxSize(tSampleMaxSize);

		archiver->inout("transport", transport);
		if(transport == "")
		{
			transport = TRANSPORT_MC;
		}
		else if (transport != TRANSPORT_MC && transport != TRANSPORT_TCP && transport != TRANSPORT_UDP)
		{
			ExceptionMessage_T msg("Illegal transport: '");
			msg += transport;
			msg += "'. Transport for topic must be either 'multicast', 'tcp', 'udp' or left blank( = multicast)";
			throw ops::ConfigException(msg);
		}
	}

	Transport_T Topic::TRANSPORT_MC = "multicast";
	Transport_T Topic::TRANSPORT_TCP = "tcp";
	Transport_T Topic::TRANSPORT_UDP = "udp";

}
