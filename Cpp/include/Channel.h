/**
* 
* Copyright (C) 2016 Lennart Andersson.
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

#include <string>
#include "Topic.h"
#include "OPSObject.h"
#include "OPSExport.h"

namespace ops
{
    class OPS_EXPORT Channel : public OPSObject
    {
    public:
        std::string channelID;
        std::string linktype;
        std::string localInterface;		// If multicast, this specifies interface to use
        std::string domainAddress;
        int port;
        __int64 outSocketBufferSize;
        __int64 inSocketBufferSize;

        Channel();

        void serialize(ArchiverInOut* archiver);

        void populateTopic(Topic* top);

        static std::string LINKTYPE_MC;
        static std::string LINKTYPE_TCP;
        static std::string LINKTYPE_UDP;
    };
}
#endif
