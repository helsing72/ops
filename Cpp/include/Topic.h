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

#include <string>
#include "OPSObject.h"
#include "OPSExport.h"


namespace ops
{   /*template<class DataType = OPSObject>*/
	class Participant;

	class OPS_EXPORT Topic : public OPSObject
    {
		friend class Domain;
		friend class Participant;
    
	public:
        Topic(std::string namee, int portt, std::string typeIDd, std::string domainAddresss);
		Topic();

		void setDomainID(std::string domID);
		std::string getDomainID();

        void setParticipantID(std::string partID);
		std::string getParticipantID();

		void setTransport(std::string transp);
		std::string getTransport();

		std::string getName();
        std::string getTypeID();

		void setDomainAddress(std::string domainAddr);
		std::string getDomainAddress();

		void setSampleMaxSize(int size);
		int getSampleMaxSize();

		void setPort(int port);
		int getPort();

		void serialize(ArchiverInOut* archiver);

		__int64 getOutSocketBufferSize();
		void setOutSocketBufferSize(__int64 size);

		__int64 getInSocketBufferSize();
		void setInSocketBufferSize(__int64 size);

		Participant* getParticipant()
		{
			return participant;
		}

		static std::string TRANSPORT_MC;
		static std::string TRANSPORT_TCP;
		static std::string TRANSPORT_UDP;


	private:
        std::string name;

		int port;
		std::string typeID;
        std::string domainAddress;
		std::string localInterface;
		std::string participantID;
		std::string domainID;
		bool reliable;
		int sampleMaxSize;
		__int64 deadline;
		__int64 minSeparation;
		std::string transport;
		__int64 outSocketBufferSize;
		__int64 inSocketBufferSize;

		Participant* participant;
        

		
        
        
    };
}
#endif
