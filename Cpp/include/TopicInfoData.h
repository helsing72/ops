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
#ifndef ops_TopicInfoData_h
#define	ops_TopicInfoData_h

#include "OPSObject.h"
#include "Topic.h"

namespace ops
{
	/// NOTE. Must be kept in sync with other OPS language implementations
	class TopicInfoData : public OPSObject
	{
	public:
		TopicInfoData()
		{
			appendType(std::string("TopicInfoData"));
		}
		TopicInfoData(Topic& topic)
		{
			appendType(std::string("TopicInfoData"));
			name = topic.getName();
			type = topic.getTypeID();
			transport = topic.getTransport();
			address = topic.getDomainAddress();
			port = topic.getPort();
			//keys;
		}

		
		void serialize(ArchiverInOut* archiver)
		{
			OPSObject::serialize(archiver);

			archiver->inout(std::string("name"), name);
			archiver->inout(std::string("type"), type);
			archiver->inout(std::string("transport"), transport);
			archiver->inout(std::string("address"), address);
			archiver->inout(std::string("port"), port);
			archiver->inout(std::string("keys"), keys);
			//archiver->inout(std::string("filters"), filters);
			
			
		}
		virtual ~TopicInfoData(){}

	public:
		std::string name;
		std::string type;
		std::string transport;
		std::string address;
		int port;
		std::vector<std::string> keys;
		//std::vector<OPSObject*> filters;
		
		
	};


}

#endif
