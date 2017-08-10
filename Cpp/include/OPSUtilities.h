/**
* 
* Copyright (C) 2016-2017 Lennart Andersson.
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

#pragma once

#include "Topic.h"

namespace ops
{
  namespace utilities
  {

	// -------------------------------------------------------------------------------
	// Methods that work on Topic names that use the syntax 'Domain::TopicName'

	// Returns a full topic name on the format 'Domain::TopicName'
	ObjectName_T fullTopicName(ObjectName_T domainName, ObjectName_T topicName)
	{
		ops::ObjectName_T name = domainName;
		name += "::";
		name += topicName;
		return name;
	}

	// Returns the topic name part
	ObjectName_T topicName(ObjectName_T name)
	{
		ObjectName_T::size_type index1;
		if ((index1 = name.find("::")) != ObjectName_T::npos) {
			return name.substr(index1 + 2);
		}
		return name;
	}

	// Returns the domain name part
	ObjectName_T domainName(ObjectName_T name)
	{
		ObjectName_T::size_type index1;
		if ((index1 = name.find("::")) != ObjectName_T::npos) {
			return name.substr(0, index1);
		}
		return "";
	}

	// -------------------------------------------------------------------------------
	// Misc methods

	bool verifyTopicType(ops::Topic& top, TypeId_T typeName)
	{
		return (top.getTypeID() == typeName);
	}

  }

}
