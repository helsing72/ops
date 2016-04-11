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
#ifndef ops_ParticipantInfoData_h
#define	ops_ParticipantInfoData_h

#include "OPSObject.h"
#include "TopicInfoData.h"

namespace ops
{
	/// NOTE. Must be kept in sync with other OPS language implementations
	class ParticipantInfoData : public OPSObject
	{
	public:
		ParticipantInfoData()
		{
			appendType(std::string("ops.ParticipantInfoData"));
			mc_udp_port = 0;
			mc_tcp_port = 0;
		}

		
		void serialize(ArchiverInOut* archiver)
		{
			OPSObject::serialize(archiver);

			archiver->inout(std::string("name"), name);
			archiver->inout(std::string("domain"), domain);
			archiver->inout(std::string("id"), id);
			archiver->inout(std::string("ip"), ip);
			archiver->inout(std::string("languageImplementation"), languageImplementation);
			archiver->inout(std::string("opsVersion"), opsVersion);
			archiver->inout(std::string("mc_udp_port"), mc_udp_port);
			archiver->inout(std::string("mc_tcp_port"), mc_tcp_port);
			archiver->inout<TopicInfoData>(std::string("subscribeTopics"), subscribeTopics, TopicInfoData());
			archiver->inout<TopicInfoData>(std::string("publishTopics"), publishTopics, TopicInfoData());
			archiver->inout(std::string("knownTypes"), knownTypes);
			
		}
		virtual ~ParticipantInfoData(){}

	public:
		std::string name;
		std::string id;
		std::string domain;
		std::string ip;
		std::string languageImplementation;
		std::string opsVersion;
		int mc_udp_port;
		int mc_tcp_port;

		std::vector<TopicInfoData> subscribeTopics;
		std::vector<TopicInfoData> publishTopics;
		std::vector<std::string> knownTypes;
		
	};


}

#endif
