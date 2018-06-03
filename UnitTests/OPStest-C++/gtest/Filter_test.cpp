
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
