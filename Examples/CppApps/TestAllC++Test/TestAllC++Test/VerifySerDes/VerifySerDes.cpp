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
#include <signal.h>

//Include a publisher for the data type we want to publish, generated from our IDL project TestAll.
#include "TestAll/ChildDataPublisher.h"
#include "TestAll/ChildDataSubscriber.h"
//Include type support for the data types we have defined in our IDL project, generated from our IDL project TestAll.
#include "TestAll/TestAllTypeFactory.h"
#include <ops.h>
#include <MemoryMap.h>
#include <OPSConfigRepository.h>
#include "PrintArchiverOut.h"

#ifdef _WIN32
	#include <crtdbg.h>
#endif

//TestAll::BaseData::MemoryPool TestAll::BaseData::_pool(10);
//TestAll::ChildData::MemoryPool TestAll::ChildData::_pool(10);
//TestAll::Fruit::MemoryPool TestAll::Fruit::_pool(50);
//TestAll::TestData::MemoryPool TestAll::TestData::_pool(100);


bool gTestFailed = false;

template <typename T>
T AssertEQ(T val, T exp, std::string mess = "")
{
	if (val != exp) {
		gTestFailed = true;
		std::cout << "Failed: " << mess << ", value= " << val << ", expected= " << exp << std::endl;
	}
	return val;
}

template <typename T>
T AssertNEQ(T val, T exp, std::string mess = "")
{
	if (val == exp) {
		gTestFailed = true;
		std::cout << "Failed: " << mess << ", value= " << val << std::endl;
	}
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

	// BaseData
	//   std::string baseText;
	AssertEQ<std::string>(data.baseText, "");
	//   std::vector<std::string> stringOpenArr;
	AssertEQ<int>(data.stringOpenArr.size(), 0);
	//   std::string stringFixArr[5];
	for (int i = 0; i < 5; i++) AssertEQ<std::string>(data.stringFixArr[i], "");
	//   ops::strings::fixed_string<23> fixLengthString;
	AssertEQ<ops::strings::fixed_string<23>>(data.fixLengthString, "");
	//   std::vector<ops::strings::fixed_string<16>> fixLengthStringOpenArr;
	AssertEQ<int>(data.fixLengthStringOpenArr.size(), 0);
	//   ops::strings::fixed_string<16> fixLengthStringFixArr[10];
	for (int i = 0; i < 10; i++) AssertEQ<ops::strings::fixed_string<16>>(data.fixLengthStringFixArr[i], "");

	// ChildData
	AssertEQ<bool>(data.bo, false, "data.bo");
    AssertEQ<char>(data.b, 0, "data.b");
    AssertEQ<short>(data.sh, 0);
    AssertEQ<int>(data.i, 0);
    AssertEQ<int64_t>(data.l, 0);
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
	//	int64_t fls[6];
	for(int i = 0; i < 6; i++) AssertEQ<int64_t>(data.fls[i], 0);

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

	// Test fields in BaseData
	//   std::string baseText;
	AssertEQ<std::string>(data.baseText, exp.baseText, "baseText");

	//   std::vector<std::string> stringOpenArr;
	AssertEQ<int>(data.stringOpenArr.size(), exp.stringOpenArr.size(), "stringOpenArr");
	for (size_t i = 0; i < data.stringOpenArr.size(); i++) AssertEQ<std::string>(data.stringOpenArr[i], exp.stringOpenArr[i], "stringOpenArr");

	//   std::string stringFixArr[5];
	for (int i = 0; i < 5; i++) AssertEQ<std::string>(data.stringFixArr[i], exp.stringFixArr[i], "stringFixArr");

	//   ops::strings::fixed_string<23> fixLengthString;
	AssertEQ<ops::strings::fixed_string<23>>(data.fixLengthString, exp.fixLengthString, "fixLengthString");

	//   std::vector<ops::strings::fixed_string<16>> fixLengthStringOpenArr;
	AssertEQ<int>(data.fixLengthStringOpenArr.size(), exp.fixLengthStringOpenArr.size(), "fixLengthStringOpenArr");
	for (size_t i = 0; i < data.fixLengthStringOpenArr.size(); i++) AssertEQ<ops::strings::fixed_string<16>>(data.fixLengthStringOpenArr[i], exp.fixLengthStringOpenArr[i], "fixLengthStringOpenArr");

	//   ops::strings::fixed_string<16> fixLengthStringFixArr[10];
	for (int i = 0; i < 10; i++) AssertEQ<ops::strings::fixed_string<16>>(data.fixLengthStringFixArr[i], exp.fixLengthStringFixArr[i], "fixLengthStringFixArr");

	// Test fields in ChildData
	AssertEQ<bool>(data.bo, exp.bo, "data.bo");
    AssertEQ<char>(data.b, exp.b, "data.b");
    AssertEQ<short>(data.sh, exp.sh);
    AssertEQ<int>(data.i, exp.i);
    AssertEQ<int64_t>(data.l, exp.l);
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
	//	int64_t fls[6];
	for(int i = 0; i < 6; i++) AssertEQ<int64_t>(data.fls[i], exp.fls[i]);

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
	// Data for fields in BaseData
	//   std::string baseText;
	data.baseText = "dynamic string";
	//   std::vector<std::string> stringOpenArr;
	data.stringOpenArr.push_back("dyn str 1");
	data.stringOpenArr.push_back("dyn str 2");
	//   std::string stringFixArr[5];
	data.stringFixArr[0] = "dsf 0";
	data.stringFixArr[1] = "dsf 1";
	data.stringFixArr[2] = "dsf 2";
	data.stringFixArr[3] = "dsf 3";
	data.stringFixArr[4] = "dsf 4";
	//   ops::strings::fixed_string<23> fixLengthString;
	data.fixLengthString = "fixed length string";
	//   std::vector<ops::strings::fixed_string<16>> fixLengthStringOpenArr;
	data.fixLengthStringOpenArr.push_back("fix len str 1");
	data.fixLengthStringOpenArr.push_back("fix len str 2");
	//   ops::strings::fixed_string<16> fixLengthStringFixArr[10];
	data.fixLengthStringFixArr[0] = "fsf 0";
	data.fixLengthStringFixArr[1] = "fsf 1";
	data.fixLengthStringFixArr[2] = "fsf 2";
	data.fixLengthStringFixArr[3] = "fsf 3";
	data.fixLengthStringFixArr[4] = "fsf 4";
	data.fixLengthStringFixArr[5] = "fsf 5";
	data.fixLengthStringFixArr[6] = "fsf 6";
	data.fixLengthStringFixArr[7] = "fsf 7";
	data.fixLengthStringFixArr[8] = "fsf 8";
	data.fixLengthStringFixArr[9] = "fsf 9";

	// Data for fields in ChildData
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

	//	int64_t fls[6];
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

volatile bool gTerminate = false;

void SignalHandler(int signal)
{
	if (signal == SIGINT) gTerminate = true;
}

int main(int argc, const char* args[])
{
#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
	std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif
	try {
		signal(SIGINT, SignalHandler);

		std::cout << "OPS Library Compile Signature: " << ops::Participant::LibraryCompileSignature() << std::endl;
		std::cout << "OPS Header Compile Signature : " << OPS_COMPILESIGNATURE << std::endl;

		if (!ops::Participant::CheckCompileSignature()) {
			std::cout << "ERROR: Headers and compiled library are not consistent !!" << std::endl;
		}

#ifdef _WIN32
		_CrtMemState s1, s2, sd;
		_CrtMemCheckpoint(&s1);
#endif

		ops::OPSObject od0;
//		TestAll::BaseData bd0;
//		TestAll::ChildData cd0;

#ifdef _WIN32
		_CrtMemCheckpoint(&s2);
		_CrtMemDifference(&sd, &s1, &s2);
#endif

//		TestAll::BaseData& pbd1 = bd0;
//		TestAll::BaseData& pbd2 = cd0;

//		pbd1 = pbd2;
//		TestAll::BaseData bd1(pbd2);

		// ===========================================

		UNUSED(argc);
		UNUSED(args);
		TestAll::ChildData cd1;
		TestAll::ChildData cd2;
		TestAll::ChildData cd3;

#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
		std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif

		// ==============================================================
		std::cout << "Test initial state..." << std::endl;
		checkEmpty(cd1);
		checkEmpty(cd2);
		checkEmpty(cd3);

		checkObjects(cd1, cd2);
		std::cout << "Finished " << std::endl;

#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
		std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif

		std::cout << "Test cloning..." << std::endl;
		fillChildData(cd1);
		cd1.fillClone(&cd2);
		checkObjects(cd1, cd2);

		// Delete first object, recreate it and compare again
		// This to check that cd2 is really a deep clone (no common object)
	///TODO


		std::cout << "Finished " << std::endl;

		std::cout << "Test Print Archiver" << std::endl;
		{
			ops::PrintArchiverOut* prt = new ops::PrintArchiverOut(std::cout);

			prt->printObject("data", &cd1);

			delete prt;
		}
		std::cout << "Print Archiver Test Finished " << std::endl;

#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
		std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif

		// ==============================================================
		std::cout << "Serialize filled object" << std::endl;
		ops::MemoryMap map(1, 65536);
		ops::ByteBuffer buf(&map);
		ops::OPSArchiverOut ao(&buf);

		std::cout << "  GetSize()= " << buf.GetSize() << std::endl;
		ao.inout("data", cd1);
		std::cout << "  GetSize()= " << buf.GetSize() << std::endl;
		std::cout << "Serialize finished" << std::endl;

#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
		std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif

		// ==============================================================
		std::cout << "Test publish/subscribe" << std::endl;

		ops::Participant::getStaticErrorService()->addListener(new ops::ErrorWriter(std::cout));

		ops::Participant* participant = ops::Participant::getInstance("TestAllDomain");
		if (!participant) {
			std::cout << "Create participant failed. do you have ops_config.xml on your rundirectory?" << std::endl;
			throw std::exception();
		}
		participant->addTypeSupport(new TestAll::TestAllTypeFactory());

#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
		std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif

		std::cout << "Dump of configuration" << std::endl;
		{
			ops::PrintArchiverOut* prt = new ops::PrintArchiverOut(std::cout);

			prt->printObject("ops_config", participant->getConfig());

			delete prt;
		}
		std::cout << "Dump of configuration Finished " << std::endl;

		// Add an errorwriter instance to the participant to catch ev. internal OPS errors
		// We can easily write our own if we want to log data in another way.
		ops::ErrorWriter* errorWriter = new ops::ErrorWriter(std::cout);
		participant->addListener(errorWriter);

		{
			// Setup & start subscriber w polling
			ops::Topic topic = participant->createTopic("ChildTopic");
			TestAll::ChildDataSubscriber sub(topic);
			sub.start();

			// Setup & start publisher
			TestAll::ChildDataPublisher pub(topic);
			pub.start();

#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
			std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif
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
				gTestFailed = true;
			}
			sub.releaseMessageLock();

			if (flag) checkObjects(cd1, cd3);

			std::cout << "Finished " << std::endl;

#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
			std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif

			std::cout << "Waiting for more data... (Press Ctrl-C to terminate)" << std::endl;
			while (!gTerminate) {
				if (sub.waitForNewData(100)) {
					std::cout << "Received new data. Checking..." << std::endl;
					sub.getData(cd3);
					sub.aquireMessageLock();
					AssertEQ<int>(sub.getMessage()->spareBytes.size(), 0, "spareBytes");
					sub.releaseMessageLock();
					checkObjects(cd3, cd1);
					std::cout << "Data check done" << std::endl;
				}
			}
		}

#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
		std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif

		delete participant;
		ops::OPSConfigRepository::Instance()->DebugTotalClear();

	}
	catch (std::exception& e) {
		std::cout << "Caugth std:exception: " << e.what() << std::endl;
		gTestFailed = true;
	}
#if defined(USE_C11) && defined(DEBUG_OPSOBJECT_COUNTER)
	std::cout << "ops::OPSObject::NumOpsObjects(): " << ops::OPSObject::NumOpsObjects() << std::endl;
#endif

	if (gTestFailed) {
		std::cout << std::endl << "T e s t   F a i l e d" << std::endl;
	} else {
		std::cout << std::endl << "T e s t   O k" << std::endl;
	}

	return 0;
}
