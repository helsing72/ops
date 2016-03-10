#include <gtest/gtest.h>
#include "TestSerializeAndDeserialize.h"
#include "TestPizza.cpp"
#include "Init_Data.h"


using namespace std;

pizza::VessuvioData vessuvio;
pizza::special::ExtraAllt extraAllt;


class Test_OPSSerialization : public testing::Test
{
public:
	Test_OPSSerialization() {
    	vessuvioObject 		= NULL;
     	vessuvioMessage 	= NULL;
     	recreatedVessuvio 	= NULL;
    	extraAlltObject 	= NULL;
    	extraAlltMessage 	= NULL;
    	recreatedExtraAllt 	= NULL;
	}

    ~Test_OPSSerialization() {}

    ops::OPSObject* vessuvioObject;
    ops::OPSMessage* vessuvioMessage;
    pizza::VessuvioData* recreatedVessuvio;

	ops::OPSObject* extraAlltObject;
	ops::OPSMessage* extraAlltMessage;
	pizza::special::ExtraAllt* recreatedExtraAllt;

};

TEST_F(Test_OPSSerialization, test_Vesuvio_OPSObject) {
	ops::MemoryMap map(1, 10000);
	ops::ByteBuffer buf(&map);

	ops::SerializableInheritingTypeFactory fact;
	fact.add(new PizzaProject::PizzaProjectTypeFactory());

	ops::OPSArchiverOut out(&buf);
	ops::OPSArchiverIn  in(&buf, &fact);

	out.inout(std::string("data"), &vessuvio);

	EXPECT_EQ(buf.GetSize(), 89);

	buf.Reset();

	EXPECT_EQ(buf.GetSize(), 0);

	recreatedVessuvio = dynamic_cast<pizza::VessuvioData*>(in.inout(std::string("data"), recreatedVessuvio));

	EXPECT_TRUE(recreatedVessuvio) << "failed creating Vessuvio object";

	test::testVessuvio(*recreatedVessuvio);

	EXPECT_EQ((int) buf.GetSize(), 89);
}

TEST_F(Test_OPSSerialization, test_Vesuvio_OPSMessage) {
	ops::MemoryMap map(1, 10000);
	ops::ByteBuffer buf(&map);

	// Setup factory to use when de-serializing a buffer
	ops::SerializableInheritingTypeFactory fact;
	fact.add(new PizzaProject::PizzaProjectTypeFactory());


	ops::OPSArchiverOut out(&buf);
	ops::OPSArchiverIn  in(&buf, &fact);


	ops::OPSMessage mess;
	mess.setDataOwner(false);
	mess.setData(&vessuvio);

	EXPECT_EQ(buf.GetSize(), 0);

	out.inout(std::string("data"), &mess);

	EXPECT_EQ((int) buf.GetSize(), 148);

	// Create object from buffer (using type info in buffer)
	buf.Reset();

	EXPECT_EQ((int) buf.GetSize(), 0);

	vessuvioObject 		= dynamic_cast<ops::OPSObject*>(in.inout(std::string("data"), vessuvioObject));
	vessuvioMessage 	= dynamic_cast<ops::OPSMessage*>(vessuvioObject);
	recreatedVessuvio 	= dynamic_cast<pizza::VessuvioData*>(vessuvioMessage->getData());

	EXPECT_TRUE(vessuvioObject);
	EXPECT_TRUE(vessuvioMessage);
	EXPECT_TRUE(recreatedVessuvio);

	test::testVessuvio(*recreatedVessuvio);

	EXPECT_EQ((int) buf.GetSize(), 148);

}

TEST_F(Test_OPSSerialization, test_ExtraAllt_OPSObject) {
	ops::MemoryMap map(1, 10000);
	ops::ByteBuffer buf(&map);

	// Setup factory to use when de-serializing a buffer
	ops::SerializableInheritingTypeFactory fact;
	fact.add(new PizzaProject::PizzaProjectTypeFactory());

	// Setup archivers
	// I can use the same buffer since I know that they aren't used at the same time
	ops::OPSArchiverOut out(&buf);
	ops::OPSArchiverIn  in(&buf, &fact);


	EXPECT_EQ(buf.GetSize(), 0);

	out.inout(std::string("data"), &extraAllt);

	EXPECT_EQ(buf.GetSize(), 797);

	// Create object from buffer (using type info in buffer)
	buf.Reset();

	EXPECT_EQ((int) buf.GetSize(), 0);

	recreatedExtraAllt = dynamic_cast<pizza::special::ExtraAllt*>(in.inout(std::string("data"), recreatedExtraAllt));

	EXPECT_TRUE(recreatedExtraAllt) << "Failed to create ExtraAllt object";

	test::testExtraAlltNormal(*recreatedExtraAllt);

	EXPECT_EQ((int) buf.GetSize(), 797);
}

TEST_F(Test_OPSSerialization, test_ExtraAllt_OPSMessage) {
	ops::MemoryMap map(1, 10000);
	ops::ByteBuffer buf(&map);

	// Setup factory to use when de-serializing a buffer
	ops::SerializableInheritingTypeFactory fact;
	fact.add(new PizzaProject::PizzaProjectTypeFactory());


	ops::OPSArchiverOut out(&buf);
	ops::OPSArchiverIn  in(&buf, &fact);

	pizza::special::ExtraAllt extraAllt;
	init::initExtraAlltNormal(extraAllt);


	ops::OPSMessage mess;
	mess.setDataOwner(false);
	mess.setData(&extraAllt);

	EXPECT_EQ(buf.GetSize(), 0);

	out.inout(std::string("data"), &mess);

	EXPECT_EQ(buf.GetSize(), 856);

	// Create object from buffer (using type info in buffer)
	buf.Reset();

	EXPECT_EQ(buf.GetSize(), 0);

	extraAlltObject 	= dynamic_cast<ops::OPSObject*>(in.inout(std::string("data"), extraAlltObject));
	extraAlltMessage 	= dynamic_cast<ops::OPSMessage*>(extraAlltObject);
	recreatedExtraAllt 	= dynamic_cast<pizza::special::ExtraAllt*>(extraAlltMessage->getData());

	EXPECT_TRUE(extraAlltObject) 	<< "Failed to create object" ;
	EXPECT_TRUE(extraAlltMessage) 	<< "Failed to create object as OPSMessage";
	EXPECT_TRUE(recreatedExtraAllt)	<< "Failed to create ExtraAllt object";

	test::testExtraAlltNormal(*recreatedExtraAllt);

	EXPECT_EQ(buf.GetSize(), 856);

}

int main(int argc, char** argv)
{
	ops::OPSMessage myMess;
	myMess.setDataOwner(true);
	init::initVessuvioData(vessuvio);
	init::initExtraAlltNormal(extraAllt);

    // required init of gtest library
    ::testing::InitGoogleTest(&argc, argv);
    int result = RUN_ALL_TESTS();

    std::cout << "RUN_ALL_TESTS returned " << result << std::endl;
    return result;
}
