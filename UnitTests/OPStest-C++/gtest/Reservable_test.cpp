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

#include "Reservable.h"
#include "ReserveInfo.h"
#include "ReferenceHandler.h"

using namespace ops;

// ===============================
// Helper classes

static int MyObject_Ctr = 0;

class MyObject : public Reservable
{
public:
	MyObject()
	{
		MyObject_Ctr++;
	}
	virtual ~MyObject()
	{
		MyObject_Ctr--;
	}
};

// ===============================

TEST(Test_Reservable, TestRefCounter) {

	EXPECT_EQ(MyObject_Ctr, 0);

	MyObject obj;

	EXPECT_EQ(MyObject_Ctr, 1);
	EXPECT_EQ(obj.getNrOfReservations(), 0);
	obj.reserve();
	EXPECT_EQ(obj.getNrOfReservations(), 1);
	obj.reserve();
	obj.reserve();
	EXPECT_EQ(obj.getNrOfReservations(), 3);
	obj.unreserve();
	EXPECT_EQ(obj.getNrOfReservations(), 2);
	obj.unreserve();
	EXPECT_EQ(obj.getNrOfReservations(), 1);
	// Note object will not be deleted when we reach 0, since we haven't provided a refHandler
	obj.unreserve();
	EXPECT_EQ(obj.getNrOfReservations(), 0);
}

TEST(Test_Reservable, TestRefHandler) {

	EXPECT_EQ(MyObject_Ctr, 0);

	ReferenceHandler ref;
	EXPECT_EQ(ref.size(), 0);

	MyObject* obj = new MyObject();
	EXPECT_EQ(MyObject_Ctr, 1);
	EXPECT_EQ(obj->getReferenceHandler(), nullptr);

	ref.addReservable(obj);
	EXPECT_EQ(obj->getReferenceHandler(), &ref);
	EXPECT_EQ(ref.size(), 1);

	obj->reserve();
	obj->reserve();
	EXPECT_EQ(obj->getNrOfReservations(), 2);

	obj->unreserve();
	EXPECT_EQ(obj->getNrOfReservations(), 1);
	EXPECT_EQ(ref.size(), 1);

	// If we now do unreserve(), obj should be deleted
	obj->unreserve();
	obj = nullptr;

	EXPECT_EQ(ref.size(), 0);
	EXPECT_EQ(MyObject_Ctr, 0);

	// Now add several objects
	MyObject* obj1 = new MyObject();
	MyObject* obj2 = new MyObject();
	MyObject* obj3 = new MyObject();
	EXPECT_EQ(MyObject_Ctr, 3);

	ref.addReservable(obj1);
	ref.addReservable(obj2);
	ref.addReservable(obj3);
	EXPECT_EQ(ref.size(), 3);

	obj1->reserve();
	obj2->reserve();
	obj3->reserve();

	obj2->unreserve(); obj2 = nullptr;
	EXPECT_EQ(ref.size(), 2);
	EXPECT_EQ(MyObject_Ctr, 2);
	obj1->unreserve(); obj1 = nullptr;
	EXPECT_EQ(ref.size(), 1);
	EXPECT_EQ(MyObject_Ctr, 1);
	obj3->unreserve(); obj3 = nullptr;
	EXPECT_EQ(ref.size(), 0);
	EXPECT_EQ(MyObject_Ctr, 0);
}

