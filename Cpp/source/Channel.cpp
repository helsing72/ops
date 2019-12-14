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

#include "OPSTypeDefs.h"
#include "Channel.h"
#include "OPSConstants.h"
#include "ConfigException.h"

namespace ops
{
    Channel::Channel()
        : channelID(""), 
        linktype(""),
        localInterface(""),
        domainAddress(""),
        timeToLive(-1),
        port(0), 
        outSocketBufferSize(-1),
        inSocketBufferSize(-1)
    {
        appendType(TypeId_T("Channel"));
    }

    void Channel::serialize(ArchiverInOut* archiver)
    {
        OPSObject::serialize(archiver);
        archiver->inout("name", channelID);
        archiver->inout("linktype", linktype);
        archiver->inout("localInterface", localInterface);
        archiver->inout("address", domainAddress);
        archiver->inout("timeToLive", timeToLive);
        archiver->inout("port", port);
        archiver->inout("outSocketBufferSize", outSocketBufferSize);
        archiver->inout("inSocketBufferSize", inSocketBufferSize);
    
        if (linktype == "")
        {
            linktype = LINKTYPE_MC;
        }
        else if (linktype != LINKTYPE_MC && linktype != LINKTYPE_TCP && linktype != LINKTYPE_UDP)
        {
			ExceptionMessage_T msg("Illegal linktype: '");
			msg += linktype;
			msg += "'. Linktype for Channel must be either 'multicast', 'tcp', 'udp' or left blank( = multicast)";
			throw ops::ConfigException(msg);
        }
    }

    void Channel::populateTopic(Topic& top) const
    {
        // If Topic doesn't specify a transport it will default to 'multicast', therefore
        // we can't just check for an empty 'top.getTransport()' to know when to replace value.
        // Therfore, if a topic is listed in a 'Transport/Channel', we assume it shall
        // use the channel values, so replace all values.
        top.setTransport(linktype);
        top.setLocalInterface(localInterface);
        top.setDomainAddress(domainAddress);
        top.setPort(port);
        top.setOutSocketBufferSize(outSocketBufferSize);
        top.setInSocketBufferSize(inSocketBufferSize);
        top.setTimeToLive(timeToLive);
        top.channelID = channelID;
    }

	Transport_T Channel::LINKTYPE_MC = "multicast";
	Transport_T Channel::LINKTYPE_TCP = "tcp";
	Transport_T Channel::LINKTYPE_UDP = "udp";
}
