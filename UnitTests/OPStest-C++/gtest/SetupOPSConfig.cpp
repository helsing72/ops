
#include <sstream>
#include "gtest/gtest.h"

#include "Participant.h"
#include "OPSConfigRepository.h"

#include "SetupOPSConfig.h"

using namespace ops;

SetupOPSConfig::SetupOPSConfig()
{
	InternalSetup();
}

void SetupOPSConfig::InternalSetup()
{
	std::string content(
		"<?xml version=\"1.0\" encoding=\"UTF - 8\"?>"
		"<!--"
		"	Description:"
		"-->"
		"<root>"
		"	<ops_config type = \"DefaultOPSConfigImpl\">"
		"		<domains>"
		"			<element type = \"Domain\">"
		"				<domainID>GTestDomain</domainID>"
		"				<domainAddress>234.5.6.8</domainAddress>"
		"				<localInterface>127.0.0.1</localInterface>"
		"				<topics>"
		"					<element type = \"Topic\">"
		"						<name>TestTopic</name>"
		"						<dataType>GTest.TestData</dataType>"
		"						<transport>tcp</transport>"
		"						<address>127.0.0.1</address>"
		"						<port>6689</port>"
		"					</element>"
		"				</topics>"
		"			</element>"
		"		</domains>"
		"	</ops_config>"
		"</root>"
	);
	std::istringstream iss(content);
	OPSConfig* config = OPSConfig::getConfig(iss);
	ASSERT_NE(config, nullptr);
	ASSERT_TRUE(OPSConfigRepository::Instance()->Add(config));
	ASSERT_FALSE(OPSConfigRepository::Instance()->Add(config));	// Duplicate test
}

SetupOPSConfig::~SetupOPSConfig() 
{
	OPSConfigRepository::Instance()->TotalClear();
}
