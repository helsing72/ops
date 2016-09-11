/**
* 
* Copyright (C) 2016 Lennart Andersson.
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
	static std::string fullTopicName(std::string domainName, std::string topicName)
	{
		return domainName + "::" + topicName;
	}

	// Returns the topic name part
	static std::string topicName(std::string name)
	{
		std::basic_string <char>::size_type index1;
		std::string s = name;
		if ((index1 = s.find("::")) != std::string::npos) {
			s.erase(0, index1+2);
		}
		return s;
	}

	// Returns the domain name part
	std::string domainName(std::string name)
	{
		std::basic_string <char>::size_type index1;
		std::string s = name;
		if ((index1 = s.find("::")) != std::string::npos) {
			s.erase(index1, std::string::npos);
			if (s != "") return s;
		}
		return "";
	}

	// -------------------------------------------------------------------------------
	// Misc methods

	bool verifyTopicType(ops::Topic& top, std::string typeName)
	{
		return (top.getTypeID() == typeName);
	}

  }

}

