/**
*
* Copyright (C) 2018-2020 Lennart Andersson.
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

#include "MemoryMap.h"

// ===============================

using namespace ops;

char dstbuf[10000];

TEST(Test_MemoryMap, DefaultConstructor) {

	MemoryMap map1;
	EXPECT_EQ(map1.getNrOfSegments(), 0) << "Wrong number of segments";
	EXPECT_EQ(map1.getSegmentSize(), 0) << "Wrong segment size";
	EXPECT_EQ(map1.getTotalSize(), 0) << "Wrong total size";

	EXPECT_THROW( map1.getSegment(0), MemoryMapException);

	char buf1[100];
	EXPECT_TRUE(map1.set(buf1, 100)) << "set failed";

	EXPECT_EQ(map1.getNrOfSegments(), 1) << "Wrong number of segments";
	EXPECT_EQ(map1.getSegmentSize(), 100) << "Wrong segment size";
	EXPECT_EQ(map1.getTotalSize(), 100) << "Wrong total size";

	char* ptr0 = nullptr;
	EXPECT_NO_THROW( ptr0 = map1.getSegment(0) );
	EXPECT_THROW( map1.getSegment(1), MemoryMapException);

	EXPECT_EQ((size_t)ptr0, (size_t)&buf1) << "Wrong segment address";

	// test copyToBytes
	for (unsigned int i = 0; i < sizeof(buf1); i++) { buf1[i] = (char)i; }
	char buffer[400];
	
	EXPECT_FALSE(map1.copyToBytes(buffer, -1, 40));
	EXPECT_FALSE(map1.copyToBytes(buffer, 50, 400));
	EXPECT_FALSE(map1.copyToBytes(buffer, 50, 40));

	EXPECT_TRUE(map1.copyToBytes(buffer, 0, 99));
	for (int i = 0; i < (99 - 0); i++) EXPECT_EQ(buffer[i], i);

	EXPECT_TRUE(map1.copyToBytes(buffer, 67, 76));
	for (int i = 0; i < (76 - 67 + 1); i++) EXPECT_EQ(buffer[i], i + 67);
}

TEST(Test_MemoryMap, WidthAndHeightConstructor) {
	// Constructor for 4 segments, each 1000 bytes
	MemoryMap map2(4, 1000);
	EXPECT_EQ(map2.getNrOfSegments(), 4) << "Wrong number of segments";
	EXPECT_EQ(map2.getSegmentSize(), 1000) << "Wrong segment size";
	EXPECT_EQ(map2.getTotalSize(), 4000) << "Wrong total size";

	EXPECT_FALSE(map2.set(dstbuf, 300)) << "set failed";

	char* ptr0 = nullptr;
	char* ptr1 = nullptr;
	char* ptr2 = nullptr;
	char* ptr3 = nullptr;
	EXPECT_NO_THROW( ptr0 = map2.getSegment(0) );
	EXPECT_NO_THROW( ptr1 = map2.getSegment(1) );
	EXPECT_NO_THROW( ptr2 = map2.getSegment(2) );
	EXPECT_NO_THROW( ptr3 = map2.getSegment(3) );
	EXPECT_THROW( map2.getSegment(4), MemoryMapException);

	EXPECT_TRUE(ptr0 != ptr1) << "Wrong segment address";
	EXPECT_TRUE(ptr0 != ptr2) << "Wrong segment address";
	EXPECT_TRUE(ptr0 != ptr3) << "Wrong segment address";

	EXPECT_TRUE(ptr1 != ptr2) << "Wrong segment address";
	EXPECT_TRUE(ptr1 != ptr3) << "Wrong segment address";

	EXPECT_TRUE(ptr2 != ptr3) << "Wrong segment address";

	// test copyToBytes
	for (int i = 0; i < map2.getSegmentSize(); i++) {
		ptr0[i] = (char)i;
		ptr1[i] = (char)(i+1);
		ptr2[i] = (char)(i+2);
		ptr3[i] = (char)(i+3);
	}
	uint8_t buffer[4000];

	EXPECT_FALSE(map2.copyToBytes((char*)buffer, -1, 40));
	EXPECT_FALSE(map2.copyToBytes((char*)buffer, 50, 4000));
	EXPECT_FALSE(map2.copyToBytes((char*)buffer, 50, 40));

	EXPECT_TRUE(map2.copyToBytes((char*)buffer, 0, 200));
	for (int i = 0; i < 200; i++) EXPECT_EQ(buffer[i], i);
	EXPECT_TRUE(map2.copyToBytes((char*)buffer, 1000, 1200));
	for (int i = 0; i < 200; i++) EXPECT_EQ(buffer[i], i+1);
	EXPECT_TRUE(map2.copyToBytes((char*)buffer, 2000, 2200));
	for (int i = 0; i < 200; i++) EXPECT_EQ(buffer[i], i+2);
	EXPECT_TRUE(map2.copyToBytes((char*)buffer, 3000, 3200));
	for (int i = 0; i < 200; i++) EXPECT_EQ(buffer[i], i+3);

	EXPECT_TRUE(map2.copyToBytes((char*)buffer, 990, 1010));
	for (int i = 0; i < 10; i++) EXPECT_EQ(buffer[i], (uint8_t)ptr0[990 + i]);
	for (int i = 0; i < 10; i++) EXPECT_EQ(buffer[i+10], (uint8_t)ptr1[i]);

	EXPECT_TRUE(map2.copyToBytes((char*)buffer, 1990, 2010));
	for (int i = 0; i < 10; i++) EXPECT_EQ(buffer[i], (uint8_t)ptr1[990 + i]);
	for (int i = 0; i < 10; i++) EXPECT_EQ(buffer[i + 10], (uint8_t)ptr2[i]);

	EXPECT_TRUE(map2.copyToBytes((char*)buffer, 2990, 3010));
	for (int i = 0; i < 10; i++) EXPECT_EQ(buffer[i], (uint8_t)ptr2[990 + i]);
	for (int i = 0; i < 10; i++) EXPECT_EQ(buffer[i + 10], (uint8_t)ptr3[i]);
}

TEST(Test_MemoryMap, SingleBufferConstructor) {
	// Constructor from single buffer
	char buf3[500];
	MemoryMap map3(buf3, sizeof(buf3));
	EXPECT_EQ(map3.getNrOfSegments(), 1) << "Wrong number of segments";
	EXPECT_EQ(map3.getSegmentSize(), 500) << "Wrong segment size";
	EXPECT_EQ(map3.getTotalSize(), 500) << "Wrong total size";

	char* ptr0 = nullptr;
	EXPECT_NO_THROW( ptr0 = map3.getSegment(0) );
	EXPECT_THROW( map3.getSegment(1), MemoryMapException);

	EXPECT_EQ((size_t)ptr0, (size_t)&buf3) << "Wrong segment address";

	char buf3B[600];
	EXPECT_TRUE(map3.set(buf3B, sizeof(buf3B))) << "set failed";

	EXPECT_EQ(map3.getNrOfSegments(), 1) << "Wrong number of segments";
	EXPECT_EQ(map3.getSegmentSize(), 600) << "Wrong segment size";
	EXPECT_EQ(map3.getTotalSize(), 600) << "Wrong total size";

	EXPECT_NO_THROW( ptr0 = map3.getSegment(0) );
	EXPECT_THROW( map3.getSegment(1), MemoryMapException);

	EXPECT_EQ((size_t)ptr0, (size_t)&buf3B) << "Wrong segment address";
}
