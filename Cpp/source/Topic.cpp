

#include "OPSTypeDefs.h"
#include "Topic.h"
#include "OPSConstants.h"
#include "ConfigException.h"

namespace ops
{
	Topic::Topic(std::string namee, int portt, std::string typeIDd, std::string domainAddresss)
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
		appendType(std::string("Topic"));

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
		appendType(std::string("Topic"));

	}
	void Topic::setParticipantID(std::string partID)
	{
		participantID = partID;
	}

	std::string Topic::getParticipantID()
	{
		return participantID;
	}

	void Topic::setDomainID(std::string domID)
	{
		domainID = domID;
	}
	std::string Topic::getDomainID()
	{
		return domainID;
	}
	std::string Topic::getName()
	{
		return name;
	}
	std::string Topic::getTypeID()
	{
		return typeID;
	}
	void Topic::setDomainAddress(std::string domainAddr)
	{
		domainAddress = domainAddr;
	}
	void Topic::setTransport(std::string transp)
	{
		transport = transp;
	}
	std::string Topic::getDomainAddress()
	{
		return domainAddress;
	}
	void Topic::setLocalInterface(std::string localIf)
	{
		localInterface = localIf;
	}
	std::string Topic::getLocalInterface()
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
	std::string Topic::getTransport()
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
		archiver->inout(std::string("name"), name);
		archiver->inout(std::string("dataType"), typeID);
		archiver->inout(std::string("port"), port);		
		archiver->inout(std::string("address"), domainAddress);

		archiver->inout(std::string("outSocketBufferSize"), outSocketBufferSize);
		archiver->inout(std::string("inSocketBufferSize"), inSocketBufferSize);
	

		//Limit this value 
		int tSampleMaxSize = getSampleMaxSize();
		archiver->inout(std::string("sampleMaxSize"), tSampleMaxSize);
		setSampleMaxSize(tSampleMaxSize);

		archiver->inout(std::string("transport"), transport);
		if(transport == "")
		{
			transport = TRANSPORT_MC;
		}
		else if (transport != TRANSPORT_MC && transport != TRANSPORT_TCP && transport != TRANSPORT_UDP)
		{
			throw ops::ConfigException(
				std::string("Illegal transport: '") + transport +
				std::string("'. Transport for topic must be either 'multicast', 'tcp', 'udp' or left blank( = multicast)"));
		}
	}

	std::string Topic::TRANSPORT_MC = "multicast";
	std::string Topic::TRANSPORT_TCP = "tcp";
	std::string Topic::TRANSPORT_UDP = "udp";


}
