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
#ifndef ops_Domain_h
#define	ops_Domain_h

#include "OPSObject.h"
#include <string>
#include <vector>
#include "Topic.h"
#include "Channel.h"
#include "Transport.h"
#include "OPSObject.h"

namespace ops
{
	class OPS_EXPORT Domain : public OPSObject
	{
		std::string domainAddress;
		int timeToLive;
		std::string localInterface;
		int inSocketBufferSize;
		int outSocketBufferSize;
		std::vector<Topic* > topics;
		std::string domainID;
		int metaDataMcPort;

		std::vector<Channel* > channels;
		std::vector<Transport* > transports;

		void checkTopicValues(Topic* top);
		void checkTransports();
		Channel* findChannel(std::string id);
		Topic* findTopic(std::string id);

	public:
		Domain();
		std::string getDomainAddress();
		virtual std::vector<Topic* > getTopics();
		virtual Topic getTopic(std::string name);
		bool existsTopic(std::string name);
		std::string getDomainID();
		int getMetaDataMcPort();
		
		void serialize(ArchiverInOut* archiver);
		int getTimeToLive();

		std::string getLocalInterface();

		int getInSocketBufferSize();
		int getOutSocketBufferSize();
		
		virtual ~Domain();
	};
}

#endif
