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
#ifndef ops_Domain_h
#define	ops_Domain_h

#include <vector>

#include "OPSTypeDefs.h"
#include "OPSObject.h"
#include "Topic.h"
#include "Channel.h"
#include "Transport.h"

namespace ops
{
	class OPS_EXPORT Domain : public OPSObject
	{
		Address_T domainAddress;
		int timeToLive;
		Address_T localInterface;
		int inSocketBufferSize;
		int outSocketBufferSize;
		std::vector<Topic* > topics;
		ObjectName_T domainID;
		int metaDataMcPort;
		int debugMcPort;
		bool optNonVirt;
		int heartbeatPeriod;
		int heartbeatTimeout;

		std::vector<Channel* > channels;
		std::vector<Transport* > transports;

		void checkTopicValues(Topic* top) const;
		void checkTransports() const;
		Channel* findChannel(ChannelId_T id) const;
		Topic* findTopic(ObjectName_T id) const;

	public:
		Domain();
		Address_T getDomainAddress() const;
		virtual std::vector<Topic* > getTopics() const;
		virtual Topic getTopic(ObjectName_T name) const;
		bool existsTopic(ObjectName_T name) const;
		ObjectName_T getDomainID() const;
		int getMetaDataMcPort() const;
		int getDebugMcPort() const;
		bool getOptNonVirt() const;
		int getHeartbeatPeriod() const;
		int getHeartbeatTimeout() const;

		void serialize(ArchiverInOut* archiver) override;
		int getTimeToLive() const;

		Address_T getLocalInterface() const;

		int getInSocketBufferSize() const;
		int getOutSocketBufferSize() const;
		
		virtual ~Domain();
	};
}
#endif
