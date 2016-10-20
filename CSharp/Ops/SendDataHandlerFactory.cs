///////////////////////////////////////////////////////////
//  SendDataHandlerFactory.cs
//  Implementation of the Class SendDataHandlerFactory
//  Created on:      12-nov-2011 09:25:35
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.IO;
using System.Collections.Generic;

namespace Ops 
{
	internal class SendDataHandlerFactory 
    {
        private Dictionary<string, ISendDataHandler> SendDataHandlers = new Dictionary<string, ISendDataHandler>();

        private McUdpSendDataHandler udpSendDataHandler = null;
        private ParticipantInfoDataListener partInfoListener = null;
        private Subscriber partInfoSub = null;

        ~SendDataHandlerFactory()
        {
            if (partInfoSub != null) partInfoSub.Stop();
            partInfoSub = null;
            partInfoListener = null;
        }

        ///LA TODO Protection ?? What if several subscribers at the same time
        /// Not needed since all calls go through the participant which is synched
        public ISendDataHandler GetSendDataHandler(Topic t, Participant participant)
        {
            // In the case that we use the same port for several topics, we need to find the sender for the transport::address::port used
            string key = t.GetTransport() + "::" + t.GetDomainAddress() + "::" + t.GetPort();
            
            if (SendDataHandlers.ContainsKey(key))
            {
                return SendDataHandlers[key];
            }

            try
            {
                // Get the local interface, doing a translation from subnet if necessary
                string localIF = Domain.DoSubnetTranslation(t.GetLocalInterface());

                ISendDataHandler sender = null;
                if (t.GetTransport().Equals(Topic.TRANSPORT_MC))
                {
                    sender = new McSendDataHandler(t, localIF, t.GetTimeToLive());
                }
                else if (t.GetTransport().Equals(Topic.TRANSPORT_TCP))
                {
                    sender = new TcpSendDataHandler(t, localIF);
                }
                if (sender != null)
                {
                    SendDataHandlers.Add(key, sender);
                    return sender;
                }

                // We don't want to add the udp handler to the handler list, so handle this last
                if (t.GetTransport().Equals(Topic.TRANSPORT_UDP))
                {
                    if (udpSendDataHandler == null)
                    {
                        // We have only one sender for all topics, so use the domain value for buffer size
                        udpSendDataHandler = new McUdpSendDataHandler(
                            participant.getDomain().GetOutSocketBufferSize(),
                            localIF);
                                                                      
                        // Setup a listener on the participant info data published by participants on our domain.
                        // We use the information for topics with UDP as transport, to know the destination for UDP sends
                        // ie. we extract ip and port from the information and add it to our McUdpSendDataHandler
                        partInfoListener = new ParticipantInfoDataListener(participant, udpSendDataHandler);

                        partInfoSub = new Subscriber(participant.CreateParticipantInfoTopic());
                        partInfoSub.newDataDefault += new NewDataDefaultEventHandler(partInfoListener.SubscriberNewData);
                        partInfoSub.Start();
                    }
                    return udpSendDataHandler;
                }

                throw new CommException("No such Transport " + t.GetTransport());
            }
            catch (System.IO.IOException ex)
            {
                throw new CommException("Error creating SendDataHandler. IOException -->" + ex.Message);
            }
        }

	}

}