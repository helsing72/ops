/**
* 
* Copyright (C) 2006-2009 Anton Gravestam.
* Copyright (C) 2019 Lennart Andersson.
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

#ifndef ops_topic_h
#define ops_topic_h

#include "OPSTypeDefs.h"
#include "OPSObject.h"
#include "OPSExport.h"

namespace ops
{
	class Participant;

	class OPS_EXPORT Topic : public OPSObject
    {
		friend class Channel;
		friend class Domain;
		friend class Participant;
    
	public:
        Topic(ObjectName_T namee, int portt, TypeId_T typeIDd, Address_T domainAddresss);
		Topic();

		void setDomainID(ObjectName_T domID);
		ObjectName_T getDomainID() const;

        void setParticipantID(ObjectName_T partID);
		ObjectName_T getParticipantID() const;

		void setTransport(Transport_T transp);
		Transport_T getTransport() const;

		ObjectName_T getName() const;
		TypeId_T getTypeID() const;

		void setDomainAddress(Address_T domainAddr);
		Address_T getDomainAddress() const;

		void setLocalInterface(Address_T localIf);
		Address_T getLocalInterface() const;

		void setSampleMaxSize(int size);
		int getSampleMaxSize() const;

		void setPort(int port);
		int getPort() const;

		void setTimeToLive(int ttl);
		int getTimeToLive() const;

		void serialize(ArchiverInOut* archiver) override;

		int64_t getOutSocketBufferSize() const;
		void setOutSocketBufferSize(int64_t size);

		int64_t getInSocketBufferSize() const;
		void setInSocketBufferSize(int64_t size);

		bool getOptNonVirt() const;
		int getHeartbeatPeriod() const;
		int getHeartbeatTimeout() const;
		ChannelId_T getChannelId() const;

		Participant* getParticipant() const
		{
			return participant;
		}

		static Transport_T TRANSPORT_MC;
		static Transport_T TRANSPORT_TCP;
		static Transport_T TRANSPORT_UDP;

	private:
        ObjectName_T name;					// Serialized

        int port{ 0 };						// Serialized
        int timeToLive{ -1 };
		TypeId_T typeID;					// Serialized
		Address_T domainAddress;			// Serialized
		Address_T localInterface;
		ObjectName_T participantID;
		ObjectName_T domainID;
		int sampleMaxSize;					// Serialized
		Transport_T transport;				// Serialized
        int64_t outSocketBufferSize{ -1 };	// Serialized
        int64_t inSocketBufferSize{ -1 };	// Serialized

        Participant* participant{ nullptr };
	
        bool optNonVirt{ false };
        int heartbeatPeriod{ 0 };
        int heartbeatTimeout{ 0 };
		ChannelId_T channelID;
	};
}
#endif
