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

/// TODO:
///   - Tests use a fixed port = 9999, which could lead to failed tests if already in use
///   - Different error cases
///   - ...

#include "gtest/gtest.h"

#include <thread>

#include "UDPSender.h"
#include "UDPReceiver.h"
#include "TCPClient.h"
#include "TCPServer.h"
#include "TCPOpsProtocol.h"
#include "TimeHelper.h"

#include "RAII_ioServ.h"
#include "listener_helpers.h"

using namespace ops;

// ===============================
// Helper

static void WaitWTimeout(Receiver& rcv, RAII_ioServ& ioServ, int64_t timeoutMs)
{
	int64_t limit = TimeHelper::currentTimeMillis() + timeoutMs;
	while ((!rcv.asyncFinished()) && (TimeHelper::currentTimeMillis() < limit)) {
		ioServ()->poll();
	}
}

static void PollWTimeout(RAII_ioServ& ioServ, int64_t timeoutMs)
{
	int64_t limit = TimeHelper::currentTimeMillis() + timeoutMs;
	while (TimeHelper::currentTimeMillis() < limit) {
		ioServ()->poll();
	}
}

static void WaitConnectedWTimeout(TCPClient& rcv, TCPServer& snd, RAII_ioServ& ioServ, int64_t timeoutMs)
{
	int64_t limit = TimeHelper::currentTimeMillis() + timeoutMs;
	while ( ((!rcv.isConnected()) || (snd.numConnected() == 0)) && (TimeHelper::currentTimeMillis() < limit)) {
		ioServ()->poll();
	}
}

// ===============================

TEST(Test_Sockets, TestTCPdefault) {

	// Create an IoService
	RAII_ioServ ioServ;
	ASSERT_NE(ioServ(), nullptr);

	// Create a listener for received data
	MyBSPListener listener(false);
	MyConnectListener cstServer("Server: ");
	MyConnectListener cstClient("Client: ");

	static const int serverPort = 9999;

	// Create TCP Server with default buffer size. Add listener for connect status
	TCPServer snd(ioServ(), "127.0.0.1", serverPort, new ops::TCPOpsProtocol());
	snd.addListener(&cstServer);
	EXPECT_EQ(snd.getNrOfListeners(), 1);

	EXPECT_STREQ(snd.getAddress().c_str(), "127.0.0.1");
	EXPECT_EQ(snd.getPort(), serverPort);

	// Create TCP Client with default buffersize
	TCPClient rcv("127.0.0.1", serverPort, ioServ(), new ops::TCPOpsProtocol());
	rcv.Notifier<BytesSizePair>::addListener(&listener);
	rcv.Notifier<ConnectStatus>::addListener(&cstClient);

	EXPECT_TRUE(rcv.asyncFinished());
	EXPECT_FALSE(rcv.isConnected());
	EXPECT_EQ(snd.numConnected(), 0);

	// Start server and client so they connect
	{
		// Need someone to drive the io service while we connect
		std::thread t1(WaitConnectedWTimeout, std::ref(rcv), std::ref(snd), std::ref(ioServ), 2000);

		// Start server
		snd.open();

		// Start client
		rcv.start();

		t1.join();
	}

	// Check connected status
	EXPECT_TRUE(rcv.isConnected());
	EXPECT_TRUE(rcv.asyncFinished());
	EXPECT_EQ(snd.numConnected(), 1);

	// Check ConnectStatus from server
	EXPECT_TRUE(cstServer.cst.connected);
	EXPECT_STREQ(cstServer.cst.addr.c_str(), rcv.getLocalAddress().c_str());
	EXPECT_EQ(cstServer.cst.port, rcv.getLocalPort());
	EXPECT_EQ(cstServer.cst.totalNo, 1);

	// Check ConnectStatus from client
	EXPECT_TRUE(cstClient.cst.connected);
	EXPECT_STREQ(cstClient.cst.addr.c_str(), snd.getAddress().c_str());
	EXPECT_EQ(cstClient.cst.port, snd.getPort());
	EXPECT_EQ(cstClient.cst.totalNo, 1);

	EXPECT_EQ(listener.counter, 1);
	EXPECT_EQ(listener.bsp.size, -5);
	EXPECT_EQ(listener.bsp.bytes, nullptr);
	EXPECT_STREQ(listener.srcIp.c_str(), snd.getAddress().c_str());
	EXPECT_EQ(listener.srcPort, snd.getPort());

	EXPECT_STREQ(rcv.getLocalAddress().c_str(), "127.0.0.1");

	// Save connected client for later use
	Address_T connectedIP = cstServer.cst.addr;
	int connectPort = cstServer.cst.port;

	// Start an async wait for data
	char rcvbuf[1024];
	rcv.asynchWait(&rcvbuf[0], 1000);
	EXPECT_FALSE(rcv.asyncFinished());

	// Send data
	char sndbuf[1024];
	EXPECT_TRUE(snd.sendTo(&sndbuf[0], 100, "", 0));

	// Wait for data
	WaitWTimeout(rcv, ioServ, 500);
	EXPECT_TRUE(rcv.isConnected());
	EXPECT_TRUE(rcv.asyncFinished());

	EXPECT_EQ(listener.counter, 2);
	EXPECT_EQ(listener.bsp.size, 100);
	EXPECT_EQ(listener.bsp.bytes, &rcvbuf[0]);
	EXPECT_STREQ(listener.srcIp.c_str(), snd.getAddress().c_str());
	EXPECT_EQ(listener.srcPort, snd.getPort());

	// Disconnect receiver. Need to send data to discover disconnect
	rcv.stop();
	EXPECT_TRUE(snd.sendTo(&sndbuf[0], 10, "", 0));
	PollWTimeout(ioServ, 100);

	EXPECT_TRUE(snd.sendTo(&sndbuf[10], 90, "", 0));
	PollWTimeout(ioServ, 100);

	EXPECT_FALSE(cstServer.cst.connected);
	EXPECT_STREQ(cstServer.cst.addr.c_str(), connectedIP.c_str());
	EXPECT_EQ(cstServer.cst.port, connectPort);
	EXPECT_EQ(cstServer.cst.totalNo, 0);

	// Need someone to drive the io service when we call stop() otherwise it will be a deadlock
	{
		std::thread t1(PollWTimeout, std::ref(ioServ), 500);
		rcv.stop();
		snd.close();
		t1.join();
	}
}
