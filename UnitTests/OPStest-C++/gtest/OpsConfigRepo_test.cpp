
// TODO:
//

#include <cstdio>
#include <fstream>

#include "gtest/gtest.h"

#include "OPSConfigRepository.h"

#include "SetupOPSConfig.h"

using namespace ops;

// ===============================

TEST(Test_OPSConfigRepo, Test) {

	// Check empty repo
	OPSConfigRepository* repo = OPSConfigRepository::Instance();
	ASSERT_NE(repo, nullptr);
	EXPECT_EQ(repo->numDomains(), 0);
	EXPECT_FALSE(repo->domainExist("GTestDomain"));

	// Check repo built from OPSConfig object
	{
		SetupOPSConfig soc;
		EXPECT_EQ(repo->numDomains(), 1);
		EXPECT_FALSE(repo->domainExist("Test"));
		EXPECT_NE(repo->getConfig(""), nullptr);
		EXPECT_EQ(repo->getConfig("Test"), nullptr);
		EXPECT_TRUE(repo->domainExist("GTestDomain"));
		OPSConfig* cfg = repo->getConfig("GTestDomain");
		ASSERT_NE(cfg, nullptr);
	}
	EXPECT_EQ(repo->numDomains(), 0);

	{
		SetupOPSConfig soc;
		EXPECT_EQ(repo->numDomains(), 1);
		EXPECT_TRUE(repo->domainExist("GTestDomain"));
		repo->Clear();
		EXPECT_EQ(repo->numDomains(), 0);
	}

	// Check repo built from file

	///TODO



	/// TODO hur göra med default fil "ops_config.xml"

}

