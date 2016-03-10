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

#ifndef SendTransportH
#define	SendTransportH

#include "UDPSender.h"
#include "SendDiscoveryModule.h"
#include "OPSObjectHelper.h"
#include "AckData.h"
#include "AckDataHelper.h"


namespace ops
{
class SendTransport
{

public:
    static SendTransport* getDefaultTransport(std::string d);
	SendTransport(std::string domain);
    virtual ~SendTransport();
	
	void send(const char* bytes, int size, std::string topic);
    AckData sendWithAck(const char* bytes, int size, std::string topic, std::string destination, int timeout);

    std::string getSocketInterfaceID();


private:
	UDPSender sender;
    SendDiscoveryModule* sendDiscovery;
    static std::vector<SendTransport*> openTransports;

    std::string domain;
    std::string getDomain();
    bool containsDestination(const TopicInterfaceData& topicData, const std::string& dest);

    



};

}
#endif
