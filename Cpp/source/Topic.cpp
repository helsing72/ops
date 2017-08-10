

#include "Topic.h"
#include "OPSConstants.h"
#include "Participant.h"
#include "ConfigException.h"

namespace ops
{
	Topic::Topic(ObjectName_T namee, int portt, TypeId_T typeIDd, Address_T domainAddresss)
		: name(namee), 
		port(portt), 
		timeToLive(-1),
		typeID(typeIDd), 
		domainAddress(domainAddresss),
		participantID("DEFAULT_PARTICIPANT"),
		reliable(false),
		sampleMaxSize(OPSConstants::PACKET_MAX_SIZE),
		deadline(OPSConstants::MAX_DEADLINE_TIMEOUT),
		minSeparation(0),
		outSocketBufferSize(-1),
		inSocketBufferSize(-1)
	{
		appendType(TypeId_T("Topic"));

	}
	Topic::Topic()
		: name(""), 
		port(0), 
		timeToLive(-1),
		typeID(""), 
		domainAddress(""),
		participantID("DEFAULT_PARTICIPANT"),
		reliable(false),
		sampleMaxSize(OPSConstants::PACKET_MAX_SIZE),
		deadline(OPSConstants::MAX_DEADLINE_TIMEOUT),
		minSeparation(0),
		outSocketBufferSize(-1),
		inSocketBufferSize(-1)
	{
		appendType(TypeId_T("Topic"));
	}

	void Topic::setParticipantID(ObjectName_T partID)
	{
		participantID = partID;
	}

	ObjectName_T Topic::getParticipantID()
	{
		return participantID;
	}

	void Topic::setDomainID(ObjectName_T domID)
	{
		domainID = domID;
	}
	ObjectName_T Topic::getDomainID()
	{
		return domainID;
	}
	ObjectName_T Topic::getName()
	{
		return name;
	}
	TypeId_T Topic::getTypeID()
	{
		return typeID;
	}
	void Topic::setDomainAddress(Address_T domainAddr)
	{
		domainAddress = domainAddr;
	}
	void Topic::setTransport(Transport_T transp)
	{
		transport = transp;
	}
	Address_T Topic::getDomainAddress()
	{
		return domainAddress;
	}
	void Topic::setLocalInterface(Address_T localIf)
	{
		localInterface = localIf;
	}
	Address_T Topic::getLocalInterface()
	{
		return localInterface;
	}
	int Topic::getSampleMaxSize()
	{
		return sampleMaxSize;
	}
	void Topic::setSampleMaxSize(int size)
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
	int Topic::getPort()
	{
		return port;
	}
	void Topic::setPort(int port)
	{
		this->port = port;
	}
	void Topic::setTimeToLive(int ttl)
	{
		timeToLive = ttl;
	}
	int Topic::getTimeToLive()
	{
		return timeToLive;
	}
	Transport_T Topic::getTransport()
	{
		return transport;
	}

	__int64 Topic::getOutSocketBufferSize()
	{
		return outSocketBufferSize;
	}
	void Topic::setOutSocketBufferSize(__int64 size)
	{
		outSocketBufferSize = size;
	}
	__int64 Topic::getInSocketBufferSize()
	{
		return inSocketBufferSize;
	}
	void Topic::setInSocketBufferSize(__int64 size)
	{
		inSocketBufferSize = size;
	}

	void Topic::serialize(ArchiverInOut* archiver)
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
