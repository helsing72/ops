// TestAll_Publish.cpp : Defines the entry point for the console application.
//
#include "TestAll/ChildData.h"

#ifdef _WIN32
  #include <Windows.h>
  #include <conio.h>
#else
  #include <stdlib.h>
#endif
#include <math.h>
#include <iostream>
#include <fstream>

//Include a publisher for the data type we want to publish, generated from our IDL project TestAll.
#include "TestAll/ChildDataPublisher.h"
#include "TestAll/ChildDataSubscriber.h"
//Include type support for the data types we have defined in our IDL project, generated from our IDL project TestAll.
#include "TestAll/TestAllTypeFactory.h"
#include <ops.h>
#include <MemoryMap.h>

template <typename T>
T AssertEQ(T val, T exp, std::string mess = "")
{
	if (val != exp) std::cout << "Failed: " << mess << ", value= " << val << ", expected= " << exp << std::endl;
	return val;
}

template <typename T>
T AssertNEQ(T val, T exp, std::string mess = "")
{
	if (val == exp) std::cout << "Failed: " << mess << ", value= " << val << std::endl;
	return val;
}

void checkEmpty(TestAll::TestData& data)
{
	AssertEQ<std::string>(data.text, "");
	AssertEQ<double>(data.value, 0.0);
}

void checkEmpty(TestAll::ChildData& data)
{
	std::cout << "Checking empty object..." << std::endl;
	
	AssertEQ<bool>(data.bo, false, "data.bo");
    AssertEQ<char>(data.b, 0, "data.b");
    AssertEQ<short>(data.sh, 0);
    AssertEQ<int>(data.i, 0);
    AssertEQ<__int64>(data.l, 0);
    AssertEQ<float>(data.f, 0.0);
    AssertEQ<double>(data.d, 0.0);
	AssertEQ<std::string>(data.s, "");

	checkEmpty(data.test2);

	AssertNEQ<void*>(data.testPointer, NULL);
	checkEmpty(*data.testPointer);

	AssertEQ<int>(data.fruit.value, TestAll::Fruit::APPLE);

	AssertEQ<int>(data.bos.size(), 0);
	//	bool fbos[11];
	for(int i = 0; i < 11; i++) AssertEQ<bool>(data.fbos[i], false);

	AssertEQ<int>(data.bs.size(), 0);
	//	char fbs[256];
	for(int i = 0; i < 256; i++) AssertEQ<char>(data.fbs[i], 0);

	AssertEQ<int>(data.shs.size(), 0);
	//	short fshs[4];
	for(int i = 0; i < 4; i++) AssertEQ<short>(data.fshs[i], 0);

	AssertEQ<int>(data.is_.size(), 0);
	//	int fis_[3];
	for(int i = 0; i < 3; i++) AssertEQ<int>(data.fis_[i], 0);

	AssertEQ<int>(data.ls.size(), 0);
	//	__int64 fls[6];
	for(int i = 0; i < 6; i++) AssertEQ<__int64>(data.fls[i], 0);

	AssertEQ<int>(data.fs.size(), 0);
	//    float ffs[77];
	for(int i = 0; i < 77; i++) AssertEQ<float>(data.ffs[i], 0.0);

	AssertEQ<int>(data.ds.size(), 0);
	//    double fds[5];
	for(int i = 0; i < 5; i++) AssertEQ<double>(data.fds[i], 0.0);

	AssertEQ<int>(data.ss.size(), 0);
	//    std::string fss[10];
	for(int i = 0; i < 10; i++) AssertEQ<std::string>(data.fss[i], "");

	AssertEQ<int>(data.test2s.size(), 0);
	//    TestData* ftest2s[5];
	for(int i = 0; i < 5; i++) checkEmpty(*data.ftest2s[i]);

	AssertEQ<int>(data.secondVirtArray.size(), 0);
	//    TestData* fsecondVirtArray[7];
	for(int i = 0; i < 7; i++) checkEmpty(*data.fsecondVirtArray[i]);

	AssertEQ<int>(data.test2s2.size(), 0);
	//    TestData ftest2s2[4];
	for(int i = 0; i < 4; i++) checkEmpty(data.ftest2s2[i]);

	AssertEQ<int>(data.fruitarr.size(), 0);
	//    Fruit ffruitarr[15];
	for(int i = 0; i < 15; i++) AssertEQ<int>(data.ffruitarr[i].value, TestAll::Fruit::APPLE);
}

void checkObjects(TestAll::TestData& data, TestAll::TestData& exp)
{
	AssertEQ<std::string>(data.text, exp.text);
	AssertEQ<double>(data.value, exp.value);
}

void checkObjects(TestAll::ChildData& data, TestAll::ChildData& exp)
{
	std::cout << "Comparing objects object..." << std::endl;
	
	AssertEQ<bool>(data.bo, exp.bo, "data.bo");
    AssertEQ<char>(data.b, exp.b, "data.b");
    AssertEQ<short>(data.sh, exp.sh);
    AssertEQ<int>(data.i, exp.i);
    AssertEQ<__int64>(data.l, exp.l);
    AssertEQ<float>(data.f, exp.f);
    AssertEQ<double>(data.d, exp.d);
	AssertEQ<std::string>(data.s, exp.s);

	checkObjects(data.test2, exp.test2);

	AssertNEQ<void*>(data.testPointer, NULL, "data.testPointer");
	AssertNEQ<void*>(exp.testPointer, NULL, "exp.testPointer");
	AssertNEQ<void*>(data.testPointer, exp.testPointer, "data.testPointer");
	AssertNEQ<void*>(dynamic_cast<TestAll::TestData*>(data.testPointer), NULL, "data.testPointer");
	AssertNEQ<void*>(dynamic_cast<TestAll::TestData*>(exp.testPointer), NULL, "data.testPointer");

	checkObjects(*data.testPointer, *exp.testPointer);

	AssertEQ<int>(data.fruit.value, exp.fruit.value);

	AssertEQ<int>(data.bos.size(), exp.bos.size());
	//	bool fbos[11];
	for(int i = 0; i < 11; i++) AssertEQ<bool>(data.fbos[i], exp.fbos[i]);

	AssertEQ<int>(data.bs.size(), exp.bs.size());
	//	char fbs[256];
	for(int i = 0; i < 256; i++) AssertEQ<char>(data.fbs[i], exp.fbs[i]);

	AssertEQ<int>(data.shs.size(), exp.shs.size());
	//	short fshs[4];
	for(int i = 0; i < 4; i++) AssertEQ<short>(data.fshs[i], exp.fshs[i]);

	AssertEQ<int>(data.is_.size(), exp.is_.size());
	//	int fis_[3];
	for(int i = 0; i < 3; i++) AssertEQ<int>(data.fis_[i], exp.fis_[i]);

	AssertEQ<int>(data.ls.size(), exp.ls.size());
	//	__int64 fls[6];
	for(int i = 0; i < 6; i++) AssertEQ<__int64>(data.fls[i], exp.fls[i]);

	AssertEQ<int>(data.fs.size(), exp.fs.size());
	//    float ffs[77];
	for(int i = 0; i < 77; i++) AssertEQ<float>(data.ffs[i], exp.ffs[i]);

	AssertEQ<int>(data.ds.size(), exp.ds.size());
	//    double fds[5];
	for(int i = 0; i < 5; i++) AssertEQ<double>(data.fds[i], exp.fds[i]);

	AssertEQ<int>(data.ss.size(), exp.ss.size());
	//    std::string fss[10];
	for(int i = 0; i < 10; i++) AssertEQ<std::string>(data.fss[i], exp.fss[i]);

	AssertEQ<int>(data.test2s.size(), exp.test2s.size());
	//    TestData* ftest2s[5];
	for(int i = 0; i < 5; i++) checkObjects(*data.ftest2s[i], *exp.ftest2s[i]);

	AssertEQ<int>(data.secondVirtArray.size(), exp.secondVirtArray.size());
	//    TestData* fsecondVirtArray[7];
	for(int i = 0; i < 7; i++) checkObjects(*data.fsecondVirtArray[i], *exp.fsecondVirtArray[i]);

	AssertEQ<int>(data.test2s2.size(), exp.test2s2.size());
	//    TestData ftest2s2[4];
	for(int i = 0; i < 4; i++) checkObjects(data.ftest2s2[i], exp.ftest2s2[i]);

	AssertEQ<int>(data.fruitarr.size(), exp.fruitarr.size());
	//    Fruit ffruitarr[15];
	for(int i = 0; i < 15; i++) AssertEQ<int>(data.ffruitarr[i].value, exp.ffruitarr[i].value);
}

// Fill ChildData with fixed values used for tests between languages
void fillChildData(TestAll::ChildData& data)
{
	data.bo = true;
    data.b = 7;
    data.sh = -99;
    data.i = 19;
    data.l = 3456789;
    data.f = 123.4567f;
    data.d = 987.12345678901;
    data.s = "Test of [de]serializing";

	data.test2.text = "TestData";
	data.test2.value = 555.5;

	data.testPointer->text = "TestPtr";
	data.testPointer->value = 777.7;

	data.fruit.value = TestAll::Fruit::PEAR;

	data.bos.push_back(false);
	data.bos.push_back(true);
	data.bos.push_back(false);

	//	bool fbos[11];
	memset(&data.fbos, 0, sizeof(data.fbos));
	data.fbos[5] = true;
	data.fbos[10] = true;

	data.bs.push_back(10);
	data.bs.push_back(20);
	data.bs.push_back(30);

	//	char fbs[256];
	for(int i = 0; i < 256; i++) data.fbs[i] = (char)i;

	data.shs.push_back(1111);
	data.shs.push_back(2222);

	//	short fshs[4];
	data.fshs[0] =  21;
	data.fshs[1] = 121;
	data.fshs[2] = 221;
	data.fshs[3] = 321;

	data.is_.push_back(100000);
	data.is_.push_back(101010);
	data.is_.push_back(110101);
	data.is_.push_back(111111);

	//	int fis_[3];
	data.fis_[0] = -1;
	data.fis_[1] = -2;
	data.fis_[2] = -3;

	data.ls.push_back(9);
	data.ls.push_back(8);
	data.ls.push_back(7);
	data.ls.push_back(6);

	//	__int64 fls[6];
	data.fls[0] = 9999;
	data.fls[1] = 9998;
	data.fls[2] = 9997;
	data.fls[3] = 9996;
	data.fls[4] = 9995;
	data.fls[5] = 9994;

	data.fs.push_back(3.1f);
	data.fs.push_back(31.14f);
	data.fs.push_back(4.56f);
	data.fs.push_back(987.0f);

	//    float ffs[77];
	for(int i = 0; i < 77; i++) data.ffs[i] = 0.0;
	data.ffs[21] = 3.1415f;

	data.ds.push_back(1.987654321);
	data.ds.push_back(2.3456789);

	//    double fds[5];
	data.fds[0] = 1.1;
	data.fds[1] = 2.1;
	data.fds[2] = 3.1;
	data.fds[3] = 4.1;
	data.fds[4] = 5.1;

	data.ss.push_back("Index 0");
	data.ss.push_back("Index 1");
	data.ss.push_back("Index 2");

	//    std::string fss[10];
	data.fss[4] = "4 string";
	data.fss[7] = "7 string";
	data.fss[9] = "9 string";

	//    std::vector<TestData*> test2s;
	data.test2s.push_back(new TestAll::TestData());
	data.test2s.push_back(new TestAll::TestData());
	data.test2s.push_back(new TestAll::TestData());
	data.test2s.push_back(new TestAll::TestData());
	//    TestData* ftest2s[5];
	data.ftest2s[2]->text = "Index 2";
	data.ftest2s[2]->value = 7.7;

	//    std::vector<TestData*> secondVirtArray;
	data.secondVirtArray.push_back(new TestAll::TestData());
	data.secondVirtArray.push_back(new TestAll::TestData());
	//    TestData* fsecondVirtArray[7];
	data.fsecondVirtArray[5]->text = "Index 5";
	data.fsecondVirtArray[5]->value = -9.99;

	//    std::vector<TestData> test2s2;
	data.test2s2.reserve(11);
	data.test2s2.resize(11);
	//    TestData ftest2s2[4];
	data.ftest2s2[3].text = "";
	data.ftest2s2[1].value = 710.6;

	data.fruitarr.reserve(2);
	data.fruitarr.resize(2);
	data.fruitarr[0].value = TestAll::Fruit::PEAR;
	data.fruitarr[1].value = TestAll::Fruit::BANANA;

	//    Fruit ffruitarr[15];
	data.ffruitarr[0].value = TestAll::Fruit::PEAR;
	data.ffruitarr[14].value = TestAll::Fruit::PEAR;
}

int main(int argc, const char* args[])
{
	TestAll::ChildData cd1;
	TestAll::ChildData cd2;
	TestAll::ChildData cd3;

	std::cout << "Test initial state..." << std::endl;
	checkEmpty(cd1);
	checkEmpty(cd2);
	checkEmpty(cd3);

	checkObjects(cd1, cd2);
	std::cout << "Finished " << std::endl;


	std::cout << "Test cloning..." << std::endl;
	fillChildData(cd1);
	cd1.fillClone(&cd2);

	checkObjects(cd1, cd2);

	// Delete first object, recreate it and compare again
	// This to check that cd2 is really a deep clone (no common object)
///TODO


	std::cout << "Finished " << std::endl;


	std::cout << "Serialize filled object" << std::endl;
	ops::MemoryMap map(1, 65536);
	ops::ByteBuffer buf(&map);
	ops::OPSArchiverOut ao(&buf);

	std::cout << "  GetSize()= " << buf.GetSize() << std::endl;
    ao.inout("data", cd1);
	std::cout << "  GetSize()= " << buf.GetSize() << std::endl;
	std::cout << "Serialize finished" << std::endl;


	std::cout << "Test publish/subscribe" << std::endl;

	ops::Participant* participant = ops::Participant::getInstance("TestAllDomain");
	if(!participant) {
		std::cout << "Create participant failed. do you have ops_config.xml on your rundirectory?" << std::endl;
		exit(1);
	}
	participant->addTypeSupport(new TestAll::TestAllTypeFactory());

	// Add an errorwriter instance to the participant to catch ev. internal OPS errors
	// We can easily write our own if we want to log data in another way.
	ops::ErrorWriter* errorWriter = new ops::ErrorWriter(std::cout);
	participant->addListener(errorWriter);

	// Setup & start subscriber w polling
	ops::Topic topic = participant->createTopic("ChildTopic");
	TestAll::ChildDataSubscriber sub(topic);
	sub.start();

	// Setup & start publisher
	TestAll::ChildDataPublisher pub(topic);
	pub.start();

	// Publish data
	pub.write(cd1);

	// Check that sent data isn't affected by publish
	checkObjects(cd1, cd2);

	// Check received values against sent values
	AssertEQ<bool>(sub.waitForNewData(100), true, "No data received");
	bool flag = AssertEQ<bool>(sub.getData(cd3), true, "No received data");

	sub.aquireMessageLock();
	if (sub.getMessage()) {
		AssertEQ<int>(sub.getMessage()->spareBytes.size(), 0, "spareBytes");
	} else {
		std::cout << "Failed: sub.getMessage() == NULL" << std::endl;
	}
	sub.releaseMessageLock();

	if (flag) checkObjects(cd1, cd3);

	std::cout << "Finished " << std::endl;

	std::cout << "Waiting for more data... (Press Ctrl-C to terminate)" << std::endl;
	while(true) {
		if (sub.waitForNewData(100)) {
			std::cout << "Received new data. Checking..." << std::endl;
			sub.getData(cd3);
			sub.aquireMessageLock();
			AssertEQ<int>(sub.getMessage()->spareBytes.size(), 0, "spareBytes");
			sub.releaseMessageLock();
			checkObjects(cd1, cd3);
			std::cout << "Data check OK" << std::endl;
		}
	}

	return 0;
}

