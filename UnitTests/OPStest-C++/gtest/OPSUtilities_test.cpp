/**
*
* Copyright (C) 2018 Lennart Andersson.
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

#include "gtest/gtest.h"

#include "OPSUtilities.h"
#include "Topic.h"

// ===============================

using namespace ops;
using namespace utilities;

TEST(Test_Utilities, Test) {

	// DomainName::TopicName
	EXPECT_STREQ(fullTopicName("Domain", "Topic").c_str(), "Domain::Topic");

	EXPECT_STREQ(topicName("Kalle::Name").c_str(), "Name");
	EXPECT_STREQ(topicName("Name").c_str(), "Name");

	EXPECT_STREQ(domainName("Kalle::Name").c_str(), "Kalle");
	EXPECT_STREQ(domainName("Name").c_str(), "");

	Topic top("name", 9999, "topicTypeId", "domainAddress");

	EXPECT_TRUE(verifyTopicType(top, "topicTypeId"));
	EXPECT_FALSE(verifyTopicType(top, "typeid"));
}
