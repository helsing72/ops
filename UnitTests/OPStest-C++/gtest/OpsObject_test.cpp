
// TODO:
// *  NumOpsObjects when USE_C11 && DEBUG_OPSOBJECT_COUNTER is defined

#include "gtest/gtest.h"

#include "OPSObject.h"
#include "OPSArchiverOut.h"

using namespace ops;

// ===============================
// Helper classes

class MyOpsObject : public OPSObject
{
public:
	MyOpsObject(): OPSObject()
	{ 
		appendType("MyOpsObject"); 
	}
};

// ===============================

TEST(Test_OPSObject, Test) {

	// Default constructed 
	OPSObject obj1;
	EXPECT_STREQ(obj1.getTypeString().c_str(), "");
	EXPECT_STREQ(obj1.getKey().c_str(), "k");

	obj1.setKey("Kalle");
	EXPECT_STREQ(obj1.getKey().c_str(), "Kalle");

	// Test appendType
	MyOpsObject obj2;
	EXPECT_STREQ(obj2.getTypeString().c_str(), "MyOpsObject ");
	EXPECT_STREQ(obj2.getKey().c_str(), "k");

	obj2.setKey("Pelle");
	EXPECT_STREQ(obj2.getKey().c_str(), "Pelle");

	// Test Clone
	obj2.spareBytes.push_back('a');
	obj2.spareBytes.push_back('b');
	obj2.spareBytes.push_back('c');
	obj2.spareBytes.push_back('d');
	obj2.spareBytes.push_back('\0');

	OPSObject* obj3 = obj2.clone();
	ASSERT_NE(obj3, nullptr);
	EXPECT_STREQ(obj3->getTypeString().c_str(), "MyOpsObject ");
	EXPECT_STREQ(obj3->getKey().c_str(), "Pelle");

	EXPECT_EQ(obj3->spareBytes.size(), 5);
	EXPECT_STREQ((char*)&obj3->spareBytes[0], "abcd");

	delete obj3;

	// Test Serialize
	// Note, Only key of OPSObject is serialized and MyOpsObject don't have any fields that is serialized
	MemoryMap map(1, 60);
	ByteBuffer buf(&map);
	OPSArchiverOut arc(&buf);
	EXPECT_EQ(buf.GetSize(), 0);
	EXPECT_EQ(buf.GetIndex(), 0);

	obj2.serialize(&arc);
	EXPECT_EQ(buf.GetSize(), 9);
	EXPECT_EQ(buf.GetIndex(), 9);

	buf.ResetIndex();
	EXPECT_STREQ(buf.ReadString().c_str(), "Pelle");
}
