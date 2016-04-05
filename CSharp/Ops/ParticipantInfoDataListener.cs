///////////////////////////////////////////////////////////
//  ParticipantInfoDataListener.cs
//  Implementation of the Class ParticipantInfoDataListener
//  Created on:      07-jan-2013 
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections.Generic;
using System.ComponentModel;

namespace Ops
{
	public class ParticipantInfoDataListener
	{
        private Participant participant;
        private McUdpSendDataHandler udpSendDataHandler;

        public ParticipantInfoDataListener(Participant part, McUdpSendDataHandler sendDataHandler)
		{
            this.participant = part;
            this.udpSendDataHandler = sendDataHandler;
		}

        ~ParticipantInfoDataListener()
        {
            this.participant = null;
        }

        public void SubscriberNewData(Subscriber sender, OPSObject data)
        {
            if (!(data is ParticipantInfoData)) return;

            ParticipantInfoData partInfo = (ParticipantInfoData)data;

            // Is it on our domain?
			if (partInfo.domain.Equals(participant.domainID)) {

                foreach (TopicInfoData tid in partInfo.subscribeTopics)
		        {
                    // We are only interrested in topics with UDP as transport
			        if ( (tid.transport.Equals(Topic.TRANSPORT_UDP)) && (participant.HasPublisherOn(tid.name)) )
				    {
					    udpSendDataHandler.AddSink(tid.name, partInfo.ip, partInfo.mc_udp_port);
					}
				}
            }
        }

    };

}

