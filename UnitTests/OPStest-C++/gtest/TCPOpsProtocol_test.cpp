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

#include <thread>

#include "TCPOpsProtocol.h"

using namespace ops;

// ===============================
// Helper classes

class MyTcpProtClient : public TCPProtocolCallbacks
{
public:
	TCPProtocol* _prot;

	MyTcpProtClient(TCPProtocol* prot) : _prot(prot) {}

	bool _connected = false;

	bool isConnected(TCPProtocol* prot) override
	{
		EXPECT_EQ(_prot, prot);
		return _connected;
	}

	char* _dstPtr = nullptr;
	int _toRead = 0;

	void startAsyncRead(TCPProtocol* prot, char* bytes, int size) override
	{
		EXPECT_EQ(_prot, prot);
		_dstPtr = bytes;
		_toRead = size;
	}

	BytesSizePair _event = { nullptr, 0 };

	void onEvent(TCPProtocol* prot, BytesSizePair arg) override
	{
		EXPECT_EQ(_prot, prot);
		_event = arg;
	}

	char sbuf[1024];
	int sbuf_idx = 0;

	int sendBuffer(TCPProtocol* prot, char* bytes, int size) override
	{
		EXPECT_EQ(_prot, prot);

		int space = (int)sizeof(sbuf) - sbuf_idx;
		if (size > space) return -1;

		memcpy(&sbuf[sbuf_idx], bytes, size);
		sbuf_idx += size;

		return size;
	}
};

TEST(Test_TCPProtocol, TestTCPProtocol) {

	TCPOpsProtocol prot;
	MyTcpProtClient client(&prot);

	char buffer[1024];

	// Too small buffer
	EXPECT_FALSE(prot.startReceive(buffer, 21));

	// No connected client
	EXPECT_FALSE(prot.startReceive(buffer, sizeof(buffer)));

	prot.connect(&client);

	// Buffer large enough
	EXPECT_TRUE(prot.startReceive(buffer, sizeof(buffer)));
	EXPECT_EQ(client._dstPtr, &buffer[0]);
	EXPECT_EQ(client._toRead, 22);

	// Data when no client
	prot.connect(nullptr);
	EXPECT_FALSE(prot.handleReceivedData(0, 18));
	prot.connect(&client);

	EXPECT_TRUE(prot.startReceive(buffer, sizeof(buffer)));
	EXPECT_EQ(client._dstPtr, &buffer[0]);
	EXPECT_EQ(client._toRead, 22);

	// Data when not connected
	EXPECT_FALSE(client.isConnected(&prot));
	EXPECT_FALSE(prot.handleReceivedData(0, 18));
	EXPECT_EQ(client._event.bytes, nullptr);
	EXPECT_EQ(client._event.size, -2);
	client._event.size = 0;

	EXPECT_TRUE(prot.startReceive(buffer, sizeof(buffer)));
	EXPECT_EQ(client._dstPtr, &buffer[0]);
	EXPECT_EQ(client._toRead, 22);

	// Data when connected, but with an error
	client._connected = true;
	EXPECT_TRUE(client.isConnected(&prot));
	EXPECT_FALSE(prot.handleReceivedData(1, 18));
	EXPECT_EQ(client._event.bytes, nullptr);
	EXPECT_EQ(client._event.size, 0);

	EXPECT_TRUE(prot.startReceive(buffer, sizeof(buffer)));
	EXPECT_EQ(client._dstPtr, &buffer[0]);
	EXPECT_EQ(client._toRead, 22);

	// Data when connected and no error
	EXPECT_TRUE(prot.handleReceivedData(0, 18));
	EXPECT_EQ(client._dstPtr, &buffer[18]);
	EXPECT_EQ(client._toRead, 4);

	// Data with too large length
	buffer[18] = 45;
	buffer[19] = 55;
	buffer[20] = 0;
	buffer[21] = 0;
	EXPECT_FALSE(prot.handleReceivedData(0, 4));

	EXPECT_TRUE(prot.startReceive(buffer, sizeof(buffer)));
	EXPECT_EQ(client._dstPtr, &buffer[0]);
	EXPECT_EQ(client._toRead, 22);

	// Data when connected and no error
	EXPECT_TRUE(prot.handleReceivedData(0, 18));
	EXPECT_EQ(client._dstPtr, &buffer[18]);
	EXPECT_EQ(client._toRead, 4);

	// Data with OK length
	buffer[18] = 0x45;
	buffer[19] = 0x2;
	buffer[20] = 0;
	buffer[21] = 0;
	EXPECT_TRUE(prot.handleReceivedData(0, 4));
	EXPECT_EQ(client._dstPtr, &buffer[0]);
	EXPECT_EQ(client._toRead, 0x245);

	// First 100 bytes
	EXPECT_TRUE(prot.handleReceivedData(0, 100));
	EXPECT_EQ(client._dstPtr, &buffer[100]);
	EXPECT_EQ(client._toRead, 0x245 - 100);
	EXPECT_EQ(client._event.bytes, nullptr);
	EXPECT_EQ(client._event.size, 0);

	// The rest of the requested bytes
	EXPECT_TRUE(prot.handleReceivedData(0, 0x245 - 100));
	EXPECT_EQ(client._event.bytes, &buffer[0]);
	EXPECT_EQ(client._event.size, 0x245);

	// --------------------------------------------------------

	// Send with OK buffer and amount
	memset(client.sbuf, 0, sizeof(client.sbuf));
	memset(buffer, 0, sizeof(buffer));
#pragma warning( disable : 4996 )
	strcpy(buffer, "Hej hopp i lingonskogen");

	EXPECT_EQ(prot.sendData(buffer, strlen(buffer)), (int)strlen(buffer));
	EXPECT_EQ(client.sbuf_idx, 22 + (int)strlen(buffer));

	int sndlen = *((int*)(client.sbuf + 18));
	EXPECT_EQ(sndlen, (int)strlen(buffer));


	client.sbuf[18] = '\0';
	EXPECT_STREQ(client.sbuf, "opsp_tcp_size_info");
	EXPECT_STREQ(&client.sbuf[22], "Hej hopp i lingonskogen");

	// Send with failed write of header
	client.sbuf_idx = sizeof(client.sbuf) - 10;
	EXPECT_EQ(prot.sendData(buffer, 10), -1);

	// Send with failed write of data
	client.sbuf_idx = sizeof(client.sbuf) - 50;
	EXPECT_EQ(prot.sendData(buffer, 60), -1);
}
