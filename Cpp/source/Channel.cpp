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
        port(0), 
        outSocketBufferSize(-1),
        inSocketBufferSize(-1)
    {
        appendType(std::string("Channel"));
    }

    void Channel::serialize(ArchiverInOut* archiver)
    {
        OPSObject::serialize(archiver);
        archiver->inout(std::string("name"), channelID);
        archiver->inout(std::string("linktype"), linktype);
        archiver->inout(std::string("localInterface"), localInterface);
        archiver->inout(std::string("address"), domainAddress);
        archiver->inout(std::string("port"), port);		
        archiver->inout(std::string("outSocketBufferSize"), outSocketBufferSize);
        archiver->inout(std::string("inSocketBufferSize"), inSocketBufferSize);
    
        if (linktype == "")
        {
            linktype = LINKTYPE_MC;
        }
        else if (linktype != LINKTYPE_MC && linktype != LINKTYPE_TCP && linktype != LINKTYPE_UDP)
        {
            throw ops::ConfigException(
                std::string("Illegal linktype: '") + linktype +
                std::string("'. Linktype for Channel must be either 'multicast', 'tcp', 'udp' or left blank( = multicast)"));
        }
    }

    void Channel::populateTopic(Topic* top)
    {
        if (top->getTransport() == "") top->setTransport(linktype);
        if (top->getLocalInterface() == "") top->setLocalInterface(localInterface);
        if (top->getDomainAddress() == "") top->setDomainAddress(domainAddress);
        if (top->getPort() == 0) top->setPort(port);
        if (top->getOutSocketBufferSize() < 0) top->setOutSocketBufferSize(outSocketBufferSize);
        if (top->getInSocketBufferSize() < 0) top->setInSocketBufferSize(inSocketBufferSize);
    }

    std::string Channel::LINKTYPE_MC = "multicast";
    std::string Channel::LINKTYPE_TCP = "tcp";
    std::string Channel::LINKTYPE_UDP = "udp";
}
