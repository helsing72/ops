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

#include "TopicInfoData.h"
#include "ParticipantInfoData.h"
#include "XMLArchiverIn.h"

using namespace ops;

// ===============================

TEST(Test_ParticipantInfo, TestTopicInfoData) {

	// Default constructed
	TopicInfoData obj1;
	EXPECT_STREQ(obj1.getTypeString().c_str(), "TopicInfoData ");
	EXPECT_STREQ(obj1.name.c_str(), "");
	EXPECT_STREQ(obj1.type.c_str(), "");
	EXPECT_STREQ(obj1.transport.c_str(), "");
	EXPECT_STREQ(obj1.address.c_str(), "");
	EXPECT_EQ(obj1.port, 0);
	EXPECT_EQ(obj1.keys.size(), (size_t)0);

	// Set some values for copy test below
	obj1.name = "Kalle";
	obj1.type = "Test";
	obj1.transport = "mail";
	obj1.address = "127";
	obj1.port = 6789;

	// Copy constructor
	TopicInfoData const obj2(obj1);
	EXPECT_STREQ(obj1.getTypeString().c_str(), "TopicInfoData ");
	EXPECT_STREQ(obj1.name.c_str(), "Kalle");
	EXPECT_STREQ(obj1.type.c_str(), "Test");
	EXPECT_STREQ(obj1.transport.c_str(), "mail");
	EXPECT_STREQ(obj1.address.c_str(), "127");
	EXPECT_EQ(obj1.port, 6789);
	EXPECT_EQ(obj1.keys.size(), (size_t)0);
}

TEST(Test_ParticipantInfo, TestTopicInfoData_Serialize) {

	TopicInfoData obj1;
	{
		std::string content(
			" <root>"
			"	<name>TestTopic</name>"
			"	<type>GTest.TestData</type>"
			"   <transport></transport>"
			"	<address>127.0.0.1</address>"
			"	<port>6689</port>"
			"   <keys>"
			"     <element>key1</element>"
			"     <element>key2</element>"
			"     <element>key3</element>"
			"   </keys>"
			" </root>"
			" "
		);
		std::istringstream is(content);
		XMLArchiverIn arcIn(is, "root", nullptr);
		obj1.serialize(&arcIn);
		EXPECT_STREQ(obj1.name.c_str(), "TestTopic");
		EXPECT_STREQ(obj1.type.c_str(), "GTest.TestData");
		EXPECT_STREQ(obj1.transport.c_str(), "");
		EXPECT_STREQ(obj1.address.c_str(), "127.0.0.1");
		EXPECT_EQ(obj1.port, 6689);
		ASSERT_EQ(obj1.keys.size(), (size_t)3);
		EXPECT_STREQ(obj1.keys[0].c_str(), "key1");
		EXPECT_STREQ(obj1.keys[1].c_str(), "key2");
		EXPECT_STREQ(obj1.keys[2].c_str(), "key3");
	}
}

TEST(Test_ParticipantInfo, TestParticipantInfoData) {

	// Default constructed
	ParticipantInfoData obj1;
	EXPECT_STREQ(obj1.name.c_str(), "");
	EXPECT_STREQ(obj1.id.c_str(), "");
	EXPECT_STREQ(obj1.domain.c_str(), "");
	EXPECT_STREQ(obj1.ip.c_str(), "");
	EXPECT_STREQ(obj1.languageImplementation.c_str(), "");
	EXPECT_STREQ(obj1.opsVersion.c_str(), "");
	EXPECT_EQ(obj1.mc_udp_port, 0);
	EXPECT_EQ(obj1.mc_tcp_port, 0);
	EXPECT_EQ(obj1.subscribeTopics.size(), (size_t)0);
	EXPECT_EQ(obj1.publishTopics.size(), (size_t)0);
	EXPECT_EQ(obj1.knownTypes.size(), (size_t)0);
}

TEST(Test_ParticipantInfo, TestParticipantInfoData_Serialize) {

	// Default constructed
	ParticipantInfoData obj1;

	std::string content(
		" <root>"
		"	<name>Test</name>"
		"	<id>nnnn</id>"
		"	<domain>Testdomain</domain>"
		"	<ip>127.0.0.1</ip>"
		"	<languageImplementation>C++</languageImplementation>"
		"	<opsVersion>4</opsVersion>"
		"	<mc_udp_port>11111</mc_udp_port>"
		"	<mc_tcp_port>22222</mc_tcp_port>"
		"	<subscribeTopics>"
		"		<element type = \"TopicInfoData\">"
		"	        <name>TestTopic</name>"
		"	        <type>GTest.TestData</type>"
		"           <transport>tcp</transport>"
		"	        <address>127.0.0.1</address>"
		"	        <port>6689</port>"
		"		</element>"
		"	</subscribeTopics>"
		"	<publishTopics>"
		"		<element type = \"TopicInfoData\">"
		"	        <name>TestTopic2</name>"
		"	        <type>GTest.TestData</type>"
		"           <transport>udp</transport>"
		"	        <address>127.0.0.1</address>"
		"	        <port>6670</port>"
		"		</element>"
		"	</publishTopics>"
		"   <knownTypes>"
		"     <element>type1</element>"
		"     <element>type2</element>"
		"     <element>type3</element>"
		"   </knownTypes>"
		" </root>"
		" "
	);
	std::istringstream is(content);
	XMLArchiverIn arcIn(is, "root", nullptr);
	obj1.serialize(&arcIn);
	EXPECT_STREQ(obj1.name.c_str(), "Test");
	EXPECT_STREQ(obj1.id.c_str(), "nnnn");
	EXPECT_STREQ(obj1.domain.c_str(), "Testdomain");
	EXPECT_STREQ(obj1.ip.c_str(), "127.0.0.1");
	EXPECT_STREQ(obj1.languageImplementation.c_str(), "C++");
	EXPECT_STREQ(obj1.opsVersion.c_str(), "4");
	EXPECT_EQ(obj1.mc_udp_port, 11111);
	EXPECT_EQ(obj1.mc_tcp_port, 22222);

	// subscribetopics
	ASSERT_EQ(obj1.subscribeTopics.size(), (size_t)1);
	EXPECT_STREQ(obj1.subscribeTopics[0].name.c_str(), "TestTopic");
	EXPECT_STREQ(obj1.subscribeTopics[0].type.c_str(), "GTest.TestData");
	EXPECT_STREQ(obj1.subscribeTopics[0].transport.c_str(), "tcp");
	EXPECT_STREQ(obj1.subscribeTopics[0].address.c_str(), "127.0.0.1");
	EXPECT_EQ(obj1.subscribeTopics[0].port, 6689);

	// publishtopics
	ASSERT_EQ(obj1.publishTopics.size(), (size_t)1);
	EXPECT_STREQ(obj1.publishTopics[0].name.c_str(), "TestTopic2");
	EXPECT_STREQ(obj1.publishTopics[0].type.c_str(), "GTest.TestData");
	EXPECT_STREQ(obj1.publishTopics[0].transport.c_str(), "udp");
	EXPECT_STREQ(obj1.publishTopics[0].address.c_str(), "127.0.0.1");
	EXPECT_EQ(obj1.publishTopics[0].port, 6670);

	// knowntypes
	ASSERT_EQ(obj1.knownTypes.size(), (size_t)3);
	EXPECT_STREQ(obj1.knownTypes[0].c_str(), "type1");
	EXPECT_STREQ(obj1.knownTypes[1].c_str(), "type2");
	EXPECT_STREQ(obj1.knownTypes[2].c_str(), "type3");
}
