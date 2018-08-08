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

#include "OPSObject.h"
#include "FilterQoSPolicy.h"
#include "KeyFilterQoSPolicy.h"

using namespace ops;

TEST(Test_Filters, TestEmpty) {

	OPSObject obj;
	obj.setKey("kalle");

	OPSObject obj2;
	obj2.setKey("olle");

	OPSObject obj3;
	obj3.setKey("pelle");

	// Empty filter
	KeyFilterQoSPolicy filter;
	EXPECT_EQ(filter.getKeys().size(), (size_t)0);
	EXPECT_TRUE(filter.applyFilter(&obj));
	EXPECT_TRUE(filter.applyFilter(&obj2));
	EXPECT_TRUE(filter.applyFilter(&obj3));
}

TEST(Test_Filters, TestOneKey) {

	OPSObject obj;
	obj.setKey("kalle");

	OPSObject obj2;
	obj2.setKey("olle");

	OPSObject obj3;
	obj3.setKey("pelle");

	// Filter with one key
	KeyFilterQoSPolicy filter("olle");

	EXPECT_EQ(filter.getKeys().size(), (size_t)1);
	EXPECT_FALSE(filter.applyFilter(&obj));
	EXPECT_TRUE(filter.applyFilter(&obj2));
	EXPECT_FALSE(filter.applyFilter(&obj3));

	// Replace key in filter
	filter.setKey("kalle");
	EXPECT_EQ(filter.getKeys().size(), (size_t)1);
	EXPECT_TRUE(filter.applyFilter(&obj));
	EXPECT_FALSE(filter.applyFilter(&obj2));
	EXPECT_FALSE(filter.applyFilter(&obj3));

	// Replace key in filter
	filter.setKey("");
	EXPECT_EQ(filter.getKeys().size(), (size_t)1);
	EXPECT_FALSE(filter.applyFilter(&obj));
	EXPECT_FALSE(filter.applyFilter(&obj2));
	EXPECT_FALSE(filter.applyFilter(&obj3));

	// Clear filter
	filter.setKeys(std::vector<ObjectKey_T>());
	EXPECT_EQ(filter.getKeys().size(), (size_t)0);
	EXPECT_TRUE(filter.applyFilter(&obj));
	EXPECT_TRUE(filter.applyFilter(&obj2));
	EXPECT_TRUE(filter.applyFilter(&obj3));
}

TEST(Test_Filters, TestSeveralKeys) {

	OPSObject obj;
	obj.setKey("kalle");

	OPSObject obj2;
	obj2.setKey("olle");

	OPSObject obj3;
	obj3.setKey("pelle");

	std::vector<ObjectKey_T> keys;
	keys.push_back("123456");
	keys.push_back("kalle");
	keys.push_back("987");
	keys.push_back("olle");

	// Filter with several keys
	KeyFilterQoSPolicy filter(keys);

	EXPECT_EQ(filter.getKeys().size(), (size_t)4);
	EXPECT_TRUE(filter.applyFilter(&obj));
	EXPECT_TRUE(filter.applyFilter(&obj2));
	EXPECT_FALSE(filter.applyFilter(&obj3));

	// Replace keys in filter
	filter.setKey("");
	EXPECT_EQ(filter.getKeys().size(), (size_t)1);
	EXPECT_FALSE(filter.applyFilter(&obj));
	EXPECT_FALSE(filter.applyFilter(&obj2));
	EXPECT_FALSE(filter.applyFilter(&obj3));

	// Clear filter
	filter.setKeys(std::vector<ObjectKey_T>());
	EXPECT_EQ(filter.getKeys().size(), (size_t)0);
	EXPECT_TRUE(filter.applyFilter(&obj));
	EXPECT_TRUE(filter.applyFilter(&obj2));
	EXPECT_TRUE(filter.applyFilter(&obj3));

	keys.clear();
	keys.push_back("123456");
	keys.push_back("kalle");
	
	filter.setKeys(keys);
	EXPECT_EQ(filter.getKeys().size(), (size_t)2);
	EXPECT_TRUE(filter.applyFilter(&obj));
	EXPECT_FALSE(filter.applyFilter(&obj2));
	EXPECT_FALSE(filter.applyFilter(&obj3));
}
