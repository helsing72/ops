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

#ifndef ops_ReceiverH
#define ops_ReceiverH

#include "OPSTypeDefs.h"
#include "Notifier.h"
#include "IOService.h" 
#include "BytesSizePair.h"
#include "OPSExport.h"

namespace ops
{
	class OPS_EXPORT Receiver : public Notifier<BytesSizePair>
	{
	public:
		virtual ~Receiver() {}

		static Receiver* create(Address_T ip, int bindPort, IOService* ioService, Address_T localInterface = "0.0.0.0", __int64 inSocketBufferSize = 16000000);
		static Receiver* createTCPClient(Address_T ip, int port, IOService* ioService, __int64 inSocketBufferSize = 16000000);
		static Receiver* createUDPReceiver(int port, IOService* ioService, Address_T localInterface = "0.0.0.0", __int64 inSocketBufferSize = 16000000);
		
		// Set the receive buffer to use
		virtual void asynchWait(char* bytes, int size) = 0;
		virtual void start() = 0;
		virtual void stop() = 0;

		virtual int getLocalPort() = 0;
		virtual Address_T getLocalAddress() = 0;

		// Used to get the sender IP and port for a received message
		// Only safe to call in callback, before a new asynchWait() is called.
		virtual void getSource(Address_T& address, int& port) = 0;
		
	};
}
#endif
