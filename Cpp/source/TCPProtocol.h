/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
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

#pragma once

#include "BytesSizePair.h"

namespace ops
{
	class TCPProtocol;

	struct TCPProtocolCallbacks
	{
		virtual bool isConnected(TCPProtocol* prot) = 0;
		virtual void startAsyncRead(TCPProtocol* prot, char* bytes, int size) = 0;
		virtual void onEvent(TCPProtocol* prot, BytesSizePair arg) = 0;
		virtual int sendBuffer(TCPProtocol* prot, char* bytes, int size) = 0;
	};

	class TCPProtocol
	{
	protected:
		TCPProtocolCallbacks* _client;
		char* _data;
		int _maxLength;
		int _accumulatedSize;
		int _expectedSize;

		// Returns false on error
		bool startAsyncRead(int size)
		{
			if (!_client) return false;
			_accumulatedSize = 0;
			_expectedSize = size;
			_client->startAsyncRead(this, _data, _expectedSize);
			return true;
		}

		void contAsyncRead()
		{
			_client->startAsyncRead(this, _data + _accumulatedSize, _expectedSize - _accumulatedSize);
		}

		// Called when _expectedSize of bytes has been received
		// Returns false if an error is detected
		virtual bool handleData() = 0;

	public:
		TCPProtocol() :
			_client(nullptr), _data(nullptr), _maxLength(0), _accumulatedSize(0), _expectedSize(0)
		{}

		virtual ~TCPProtocol()
		{}

		// Connect a client
		void connect(TCPProtocolCallbacks* client) { _client = client; }

		// Create a fresh instance of the protocol (used by TCPServer for each connected client)
		virtual TCPProtocol* create() = 0;

		// Returning false if unable to start
		virtual bool startReceive(char* bytes, int size) = 0;

		// Returns number of bytes written or < 0 if an error
		virtual int sendData(char* buf, int size) = 0;

		// Returning false if an error is detected (should disconnect)
		virtual bool handleReceivedData(int error, int nrBytesReceived)
		{
			bool errorDetected = true;
			if (_client) {
				if (!_client->isConnected(this)) {
					_client->onEvent(this, BytesSizePair(nullptr, -2));

				} else {
					if (!error && nrBytesReceived > 0) {
						_accumulatedSize += nrBytesReceived;
						if (_accumulatedSize < _expectedSize) {
							// We have not gotten all data yet, Read more
							contAsyncRead();
							errorDetected = false;

						} else {
							// We have got requested amount (_expectedSize) of bytes
							errorDetected = !handleData();
						}
					}
				}
			}
			return !errorDetected;
		}
	};
}
