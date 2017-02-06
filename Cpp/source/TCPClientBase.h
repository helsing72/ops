/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
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

#include <string>
#include <iostream>

#include "Receiver.h"
#include "BytesSizePair.h"
#include "Participant.h"
#include "BasicError.h"

namespace ops
{

    class TCPClientBase : public Receiver
    {
    public:
        TCPClientBase() : 
			_maxLength(0), _data(nullptr), _accumulatedSize(0), _expectedSize(0), _readingHeader(true)
        {
        }

        void asynchWait(char* bytes, int size)
        {
            _data = bytes;
            _maxLength = size;

            if (isConnected()) {
                // Always start by reading the packet size when using tcp
				_readingHeader = true;
				_expectedSize = 22;
				_accumulatedSize = 0;
				startAsyncRead(_data, _expectedSize);
            }
        }

	protected:
		// Should be called by the derived classes
		void connected(bool value)
		{
			if (value) {
				notifyNewEvent(BytesSizePair(NULL, -5)); //Connection was down but has been reastablished.
			} else {

			}
		}

		virtual bool isConnected() = 0;
		virtual void startAsyncRead(char* bytes, int size) = 0;

		// Should be called by the derived classes
		void handleReceivedData(int error, int nrBytesReceived)
		{
			if (!isConnected()) {
				notifyNewEvent(BytesSizePair(NULL, -2));

			} else {
				bool errorDetected = false;

				if (!error && nrBytesReceived > 0) {
					_accumulatedSize += nrBytesReceived;
					if (_accumulatedSize < _expectedSize) {
						// We have not gotten all data yet, Read more
						startAsyncRead(_data + _accumulatedSize, _expectedSize - _accumulatedSize);
					} else {
						// We have got all data
						_accumulatedSize = 0;
						if (_readingHeader) {
							// Get size of data packet from the received size packet
							int sizeInfo = *((int*)(_data + 18));
							if (sizeInfo > _maxLength) {
								// This is an error, we are not able to receive more than _maxLength bytes (the buffer size)
								errorDetected = true;
							} else {
								_readingHeader = false;
								_expectedSize = sizeInfo;
								startAsyncRead(_data, _expectedSize);
							}
						} else {
							// Got a complete data packet
							notifyNewEvent(BytesSizePair(_data, _expectedSize));
						}
					}
				} else {
					errorDetected = true;
				}

				if (errorDetected) {
					//Report error
					ops::BasicError err("TCPClient", "handle_received_data", "Error in receive.");
					Participant::reportStaticError(&err);

					//Close the socket
					stop();

					//Notify our user
					notifyNewEvent(BytesSizePair(NULL, -1));

					//Try to connect again
					start();
				}
			}
		}

	private:
        int _maxLength;
        char* _data;
        int _accumulatedSize;
		int _expectedSize;
		bool _readingHeader;
    };
}
