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
#include "OPSTypeDefs.h"
#include "Receiver.h"

#ifndef REPLACE_TRANSPORT_LAYER

#include "MulticastReceiver.h"
#include "TCPClient.h"
#include "UDPReceiver.h"

namespace ops
{
	Receiver* Receiver::create(std::string ip, int bindPort, IOService* ioService, std::string localInterface, __int64 inSocketBufferSize)
	{
		return new MulticastReceiver(ip, bindPort, ioService, localInterface, inSocketBufferSize);
	}
	Receiver* Receiver::createTCPClient(std::string ip, int port, IOService* ioService, __int64 inSocketBufferSize)
	{
		return new TCPClient(ip, port, ioService, inSocketBufferSize);
	}
	Receiver* Receiver::createUDPReceiver(int port, IOService* ioService, std::string localInterface, __int64 inSocketBufferSize)
	{
		return new UDPReceiver(port, ioService, localInterface, inSocketBufferSize);
	}

}
#endif // REPLACE_TRANSPORT_LAYER
