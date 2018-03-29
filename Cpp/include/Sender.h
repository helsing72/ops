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

/* 
 * File:   Sender.h
 * Author: Anton Gravestam
 *
 * Created on den 22 oktober 2008, 20:01
 */

#ifndef ops_SenderH
#define	ops_SenderH

#include "OPSTypeDefs.h"
#include "ByteBuffer.h"
#include "IOService.h" 
#include "OPSExport.h"

namespace ops
{
    ///Interface used to send data
    class Sender
    {
    public:
        virtual bool sendTo(char* buf, int size, const Address_T& ip, int port) = 0;
        virtual int getPort() = 0;
        virtual Address_T getAddress() = 0;
		virtual void open() = 0;
		virtual void close() = 0;

        static OPS_EXPORT Sender* create(IOService* ioService, Address_T localInterface = "0.0.0.0", int ttl = 1, int64_t outSocketBufferSize = 16000000);
        static OPS_EXPORT Sender* createUDPSender(IOService* ioService, Address_T localInterface = "0.0.0.0", int ttl = 1, int64_t outSocketBufferSize = 16000000);
        static OPS_EXPORT Sender* createTCPServer(IOService* ioService, Address_T ip, int port, int64_t outSocketBufferSize = 16000000);

        virtual ~Sender() {}
    };

}

#endif
