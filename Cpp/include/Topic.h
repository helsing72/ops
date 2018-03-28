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
		friend class Domain;
		friend class Participant;
    
	public:
        Topic(ObjectName_T namee, int portt, TypeId_T typeIDd, Address_T domainAddresss);
		Topic();

		void setDomainID(ObjectName_T domID);
		ObjectName_T getDomainID();

        void setParticipantID(ObjectName_T partID);
		ObjectName_T getParticipantID();

		void setTransport(Transport_T transp);
		Transport_T getTransport();

		ObjectName_T getName();
		TypeId_T getTypeID();

		void setDomainAddress(Address_T domainAddr);
		Address_T getDomainAddress();

		void setLocalInterface(Address_T localIf);
		Address_T getLocalInterface();

		void setSampleMaxSize(int size);
		int getSampleMaxSize();

		void setPort(int port);
		int getPort();

		void setTimeToLive(int ttl);
		int getTimeToLive();

		void serialize(ArchiverInOut* archiver);

		__int64 getOutSocketBufferSize();
		void setOutSocketBufferSize(__int64 size);

		__int64 getInSocketBufferSize();
		void setInSocketBufferSize(__int64 size);

		Participant* getParticipant()
		{
			return participant;
		}

		static Transport_T TRANSPORT_MC;
		static Transport_T TRANSPORT_TCP;
		static Transport_T TRANSPORT_UDP;

	private:
        ObjectName_T name;					// Serialized

		int port;							// Serialized
		int timeToLive;
		TypeId_T typeID;					// Serialized
		Address_T domainAddress;			// Serialized
		Address_T localInterface;
		ObjectName_T participantID;
		ObjectName_T domainID;
		//bool reliable;
		int sampleMaxSize;					// Serialized
		//__int64 deadline;
		//__int64 minSeparation;
		Transport_T transport;				// Serialized
		__int64 outSocketBufferSize;		// Serialized
		__int64 inSocketBufferSize;			// Serialized

		Participant* participant;
    };
}
#endif
