
#include "gtest/gtest.h"

#include "Listener.h"
#include "Notifier.h"
#include "DataListener.h"
#include "DataNotifier.h"
#include "DeadlineMissedListener.h"

using namespace ops;

// ===============================
// Helper classes

class MyListener : public Listener<int>
{
public:
	int value;
	MyListener() : value(0) {}
	virtual void onNewEvent(Notifier<int>* sender, int arg) { UNUSED(sender);  value = arg; }
};

class MyNotifier : public Notifier<int>
{
public:
	void notify(int value) { notifyNewEvent(value); }
};

// ===============================

TEST(Test_NotifierListener, TestNotifierListener) {

	MyListener listener1;
	MyListener listener2;
	MyNotifier notifier;

	EXPECT_EQ(notifier.getNrOfListeners(), 0);
	EXPECT_EQ(listener1.value, 0);
	EXPECT_EQ(listener2.value, 0);

	notifier.notify(1);

	EXPECT_EQ(listener1.value, 0);
	EXPECT_EQ(listener2.value, 0);

	notifier.addListener(&listener1);

	EXPECT_EQ(notifier.getNrOfListeners(), 1);
	
	notifier.notify(10);

	EXPECT_EQ(listener1.value, 10);
	EXPECT_EQ(listener2.value, 0);

	notifier.addListener(&listener2);

	EXPECT_EQ(notifier.getNrOfListeners(), 2);

	notifier.notify(20);

	EXPECT_EQ(listener1.value, 20);
	EXPECT_EQ(listener2.value, 20);

	notifier.removeListener(&listener1);

	EXPECT_EQ(notifier.getNrOfListeners(), 1);

	notifier.notify(30);

	EXPECT_EQ(listener1.value, 20);
	EXPECT_EQ(listener2.value, 30);

	notifier.removeListener(&listener2);

	EXPECT_EQ(notifier.getNrOfListeners(), 0);

	notifier.notify(40);

	EXPECT_EQ(listener1.value, 20);
	EXPECT_EQ(listener2.value, 30);
}

// ===============================
// Helper classes

class MyDataListener : public DataListener
{
public:
	int counter;
	MyDataListener() : counter(0) {}
	virtual void onNewData(DataNotifier* sender) { UNUSED(sender); counter++; }
};

class MyDataNotifier : public DataNotifier
{
public:
	void notify() { notifyNewData(); }
};

void MyCallback(DataNotifier* sender, void* userData)
{
	UNUSED(sender);
	MyDataListener* listener = (MyDataListener*)userData;
	listener->counter++;
}

TEST(Test_NotifierListener, TestDataNotifierListener) {

	MyDataListener listener1;
	MyDataListener listener2;
	MyDataNotifier notifier;

	EXPECT_EQ(listener1.counter, 0);
	EXPECT_EQ(listener2.counter, 0);

	notifier.addDataListener(&listener1);
	notifier.notify();

	EXPECT_EQ(listener1.counter, 1);
	EXPECT_EQ(listener2.counter, 0);

	notifier.addDataListener(&listener2);
	notifier.notify();

	EXPECT_EQ(listener1.counter, 2);
	EXPECT_EQ(listener2.counter, 1);

	notifier.addDataListener(MyCallback, (void*)&listener2);
	notifier.notify();

	EXPECT_EQ(listener1.counter, 3);
	EXPECT_EQ(listener2.counter, 3);

	notifier.addDataListener(&listener1);
	notifier.notify();

	EXPECT_EQ(listener1.counter, 5);
	EXPECT_EQ(listener2.counter, 5);

	notifier.notify();

	EXPECT_EQ(listener1.counter, 7);
	EXPECT_EQ(listener2.counter, 7);
}

// ===============================
// Helper classes

class MyDeadlineListener : public DeadlineMissedListener
{
public:
	int counter;
	MyDeadlineListener() : counter(0) {}
	virtual void onDeadlineMissed(DeadlineMissedEvent* e) { UNUSED(e); counter++; }
};

TEST(Test_NotifierListener, TestDeadlineMissedListener) {

	MyDeadlineListener listener1;
	MyDeadlineListener listener2;
	DeadlineMissedEvent notifier;

	EXPECT_EQ(listener1.counter, 0);
	EXPECT_EQ(listener2.counter, 0);

	notifier.addDeadlineMissedListener(&listener1);
	notifier.notifyDeadlineMissed();

	EXPECT_EQ(listener1.counter, 1);
	EXPECT_EQ(listener2.counter, 0);

	notifier.addDeadlineMissedListener(&listener2);
	notifier.notifyDeadlineMissed();

	EXPECT_EQ(listener1.counter, 2);
	EXPECT_EQ(listener2.counter, 1);
}
