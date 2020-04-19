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
import java.util.Observer;
import java.util.Observable;

public class ParticipantInfoDataListener
{
    private Participant participant;
    private McUdpSendDataHandler udpSendDataHandler = null;
    private Subscriber partInfoSub = null;

    public ParticipantInfoDataListener(Participant part)
    {
        this.participant = part;
    }

    public void connectUdp(McUdpSendDataHandler udpSendDataHandler)
    {
        this.udpSendDataHandler = udpSendDataHandler;
    }

    public void start()
    {
        if (partInfoSub == null) {
            try {
                partInfoSub = new Subscriber(participant.createParticipantInfoTopic());
                //Add a listener to the subscriber
                partInfoSub.addObserver(new Observer() {
                    public void update(Observable o, Object arg)
                    {
                        subscriberNewData(partInfoSub, partInfoSub.getMessage().getData());
                    }
                });
                partInfoSub.start();
            }
            catch (ConfigurationException e)
            {
            }
        }
    }

    public void stop()
    {
      if (partInfoSub != null) {
          partInfoSub.stop();
          partInfoSub = null;
      }
    }

    private void subscriberNewData(Subscriber sender, OPSObject data)
    {
        if (!(data instanceof ParticipantInfoData)) return;

        ParticipantInfoData partInfo = (ParticipantInfoData)data;

        // Is it on our domain?
	      if (partInfo.domain.equals(participant.domainID)) {
            for (TopicInfoData tid : partInfo.subscribeTopics) {
                // We are only interrested in topics with UDP as transport
                if ( (tid.transport.equals(Topic.TRANSPORT_UDP)) && (participant.hasPublisherOn(tid.name)) ) {
                    try {
                        if (udpSendDataHandler != null) udpSendDataHandler.addSink(tid.name, partInfo.ip, partInfo.mc_udp_port, false);
                    }
                    catch (IOException e)
                    {
                    }
                }
            }
        }
    }

}
