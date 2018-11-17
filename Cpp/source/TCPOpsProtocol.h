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

#include "TCPProtocol.h"

namespace ops
{
	// The OPS TCP Protocol has a leading header of 22 bytes with
	//    0..17    "opsp_tcp_size_info"
	//   18..21    int32_t in little-endian format giving size of data that follows

	class TCPOpsProtocol : public TCPProtocol
	{
	protected:
		bool _readingHeader;
		char _sizeInfoBuffer[32];

		// _data[0 .. _accumulatedSize-1] contains data (_accumulatedSize == _expectedSize)
		// returns false if error
		bool handleData() override
		{
			bool errorDetected = false;
			if (_readingHeader) {
				// Get size of data packet from the received size packet
				int sizeInfo = *((int*)(_data + 18));
				if (sizeInfo > _maxLength) {
					// This is an error, we are not able to receive more than _maxLength bytes (the buffer size)
					errorDetected = true;
				} else {
					_readingHeader = false;
					errorDetected = !startAsyncRead(sizeInfo);
				}
			} else {
				// Got a complete data packet
				_client->onEvent(this, BytesSizePair(_data, _expectedSize));
			}
			return !errorDetected;
		}

	public:
		TCPOpsProtocol() :
			_readingHeader(true), _sizeInfoBuffer("opsp_tcp_size_info")
		{}

		TCPProtocol* create() override
		{
			return new TCPOpsProtocol();
		}

		// Returning false if unable to start
		bool startReceive(char* bytes, int size) override
		{
			// Always start by reading the packet size when using tcp
			// We use the users provided buffer for receiving the size info
			if (size < 22) return false;
			_data = bytes;
			_maxLength = size;
			_readingHeader = true;
			return startAsyncRead(22);
		}

		// Returns number of bytes written or < 0 if an error
		int sendData(char* bytes, int size) override
		{
			// Fill in header
			*((int*)(_sizeInfoBuffer + 18)) = size;

			// Send header
			int res = _client->sendBuffer(this, _sizeInfoBuffer, 22);
			if (res < 0) return res;

			// Send data
			return _client->sendBuffer(this, bytes, size);
		}

	};
}
