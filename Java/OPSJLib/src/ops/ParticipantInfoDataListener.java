/**
*
* Copyright (C) 2006-2009 Anton Gravestam.
* Copyright (C) 2020 Lennart Andersson.
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

package ops;

import java.io.IOException;

public class ParticipantInfoDataListener
{
    private Participant participant;
    private McUdpSendDataHandler udpSendDataHandler;

    public ParticipantInfoDataListener(Participant part, McUdpSendDataHandler sendDataHandler)
    {
        this.participant = part;
        this.udpSendDataHandler = sendDataHandler;
    }

    public void SubscriberNewData(Subscriber sender, OPSObject data)
    {
        if (!(data instanceof ParticipantInfoData)) return;

        ParticipantInfoData partInfo = (ParticipantInfoData)data;

        // Is it on our domain?
	      if (partInfo.domain.equals(participant.domainID)) {

            for (TopicInfoData tid : partInfo.subscribeTopics)
            {
                // We are only interrested in topics with UDP as transport
	              if ( (tid.transport.equals(Topic.TRANSPORT_UDP)) && (participant.hasPublisherOn(tid.name)) )
                {
		                try
                    {
                        udpSendDataHandler.addSink(tid.name, partInfo.ip, partInfo.mc_udp_port, false);
                    }
                    catch (IOException e)
                    {
                    }
		            }
            }
        }
    }

}
