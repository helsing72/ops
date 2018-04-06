
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
