/**
*
* Copyright (C) 2018-2019 Lennart Andersson.
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

// TODO: Reading the default ops configuration file "ops_config.xml" in the current working directory
//

#include <cstdio>
#include <fstream>

#include "gtest/gtest.h"

#include "OPSConfigRepository.h"

#include "CreateTempOpsConfigFile.h"
#include "SetupOPSConfig.h"

using namespace ops;

// ===============================

TEST(Test_OPSConfigRepo, Test) {

	// Check empty repo
	OPSConfigRepository* const repo = OPSConfigRepository::Instance();
	ASSERT_NE(repo, nullptr);
	EXPECT_EQ(repo->numDomains(), 0);
	EXPECT_FALSE(repo->domainExist("GTestDomain"));

	// Check repo built from OPSConfig object
	{
		SetupOPSConfig const soc;
		EXPECT_EQ(repo->numDomains(), 1);
		EXPECT_FALSE(repo->domainExist("Test"));
		EXPECT_NE(repo->getConfig(""), nullptr);
		EXPECT_EQ(repo->getConfig("Test"), nullptr);
		EXPECT_TRUE(repo->domainExist("GTestDomain"));
		OPSConfig* const cfg = repo->getConfig("GTestDomain");
		ASSERT_NE(cfg, nullptr);
	}
	EXPECT_EQ(repo->numDomains(), 0);

	{
		SetupOPSConfig const soc;
		EXPECT_EQ(repo->numDomains(), 1);
		EXPECT_TRUE(repo->domainExist("GTestDomain"));
		repo->Clear();
		EXPECT_EQ(repo->numDomains(), 0);
	}

	// Check repo built from file
	std::string ops_config, ops_config2;
	RAII_FileRemover remover;

	// Remove warnings for tmpnam with VC++
#ifdef _MSC_VER
#pragma warning(disable: 4996)
#endif

	// Try first with a non existing file
	ops_config = std::tmpnam(nullptr);
	ASSERT_FALSE(repo->Add(ops_config));
	EXPECT_EQ(repo->numDomains(), 0);

	// Create files needed below
	ASSERT_TRUE(CreateTempOpsConfigFile(ops_config));
	remover.Add(ops_config);
	ASSERT_TRUE(CreateTempOpsConfigFile(ops_config2));
	remover.Add(ops_config2);

	// Try with an existing file, selecting domains
	EXPECT_FALSE(repo->Add(ops_config, "Non-existing"));
	EXPECT_EQ(repo->numDomains(), 0);

	EXPECT_TRUE(repo->Add(ops_config, "TestDomain"));
	EXPECT_EQ(repo->numDomains(), 1);

	EXPECT_FALSE(repo->Add(ops_config, "TestDomain"));	// already exist test
	EXPECT_EQ(repo->numDomains(), 1);

	EXPECT_TRUE(repo->Add(ops_config, "DummyDomain"));
	EXPECT_EQ(repo->numDomains(), 2);

	EXPECT_NE(repo->getConfig("TestDomain"), nullptr);
	EXPECT_NE(repo->getConfig("DummyDomain"), nullptr);

	repo->TotalClear();
	EXPECT_EQ(repo->numDomains(), 0);

	// Try with an existing file, taking all domains in file
	ASSERT_TRUE(repo->Add(ops_config));
	EXPECT_EQ(repo->numDomains(), 2);

	// Try same domains from another file
	EXPECT_FALSE(repo->Add(ops_config2));
	EXPECT_EQ(repo->numDomains(), 2);

	EXPECT_NE(repo->getConfig("TestDomain"), nullptr);
	EXPECT_NE(repo->getConfig("DummyDomain"), nullptr);

	repo->TotalClear();
	EXPECT_EQ(repo->numDomains(), 0);

	/// TODO hur göra med default fil "ops_config.xml"

}

