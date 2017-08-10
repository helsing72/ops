/**
* 
* Copyright (C) 2016-2017 Lennart Andersson.
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

#ifndef ops_channel_h
#define ops_channel_h

#include "OPSTypeDefs.h"
#include "Topic.h"
#include "OPSObject.h"
#include "OPSExport.h"

namespace ops
{
    class OPS_EXPORT Channel : public OPSObject
    {
    public:
		ChannelId_T channelID;
        Transport_T linktype;
        Address_T localInterface;     // If multicast, this specifies interface to use
        Address_T domainAddress;
        int timeToLive;                 // if multicast, this specifies the ttl parameter
        int port;
        __int64 outSocketBufferSize;
        __int64 inSocketBufferSize;

        Channel();

        void serialize(ArchiverInOut* archiver);

        void populateTopic(Topic* top);

        static Transport_T LINKTYPE_MC;
        static Transport_T LINKTYPE_TCP;
        static Transport_T LINKTYPE_UDP;
    };
}
#endif
