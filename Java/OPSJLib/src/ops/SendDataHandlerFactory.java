/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ops;

import java.util.Observable;
import java.util.Observer;
import java.io.IOException;
import java.util.HashMap;

/**
 *
 * @author Anton Gravestam
 */
class SendDataHandlerFactory
{
    private HashMap<String, SendDataHandler> sendDataHandlers = new HashMap<String, SendDataHandler>();

    private McUdpSendDataHandler udpSendDataHandler = null;
    private ParticipantInfoDataListener partInfoListener = null;
    private Subscriber partInfoSub = null;

    SendDataHandler getSendDataHandler(Topic t, Participant participant) throws CommException
    {
        // In the case that we use the same port for several topics, we need to find the sender for the transport::address::port used
        String key = t.getTransport() + "::" + t.getDomainAddress() + "::" + t.getPort();

        if (sendDataHandlers.containsKey(key))
        {
            return sendDataHandlers.get(key);
        }

        SendDataHandler sender = null;
        try
        {
            String localIf = Domain.DoSubnetTranslation(participant.getDomain().getLocalInterface());

            if(t.getTransport().equals(Topic.TRANSPORT_MC))
            {
                sender = new McSendDataHandler(t, localIf, participant.getDomain().getTimeToLive());
            }
            if(t.getTransport().equals(Topic.TRANSPORT_TCP))
            {
                sender = new TcpSendDataHandler(t, localIf);
            }
            if (sender != null)
            {
                sendDataHandlers.put(key, sender);
                return sender;
            }

            // We don't want to add the udp handler to the handler list, so handle this last
            if(t.getTransport().equals(Topic.TRANSPORT_UDP))
            {
                if (udpSendDataHandler == null)
                {
                    // We have only one sender for all topics, so use the domain value for buffer size
                    udpSendDataHandler = new McUdpSendDataHandler(
                        participant.getDomain().GetOutSocketBufferSize(),
                        localIf);

                    // Setup a listener on the participant info data published by participants on our domain.
                    // We use the information for topics with UDP as transport, to know the destination for UDP sends
                    // ie. we extract ip and port from the information and add it to our McUdpSendDataHandler
                    partInfoListener = new ParticipantInfoDataListener(participant, udpSendDataHandler);

                    try
                    {
                        partInfoSub = new Subscriber(participant.createParticipantInfoTopic());
                        //Add a listener to the subscriber
                        partInfoSub.addObserver(new Observer() {
                            public void update(Observable o, Object arg)
                            {
                                if (partInfoListener != null) partInfoListener.SubscriberNewData(partInfoSub, partInfoSub.getMessage().getData());
                            }
                        });
                        partInfoSub.start();
                    }
                    catch (ConfigurationException e)
                    {
                    }
                }
                return udpSendDataHandler;
            }

            throw new CommException("No such Transport " + t.getTransport());
        }
        catch (IOException ex)
        {
            throw new CommException("Error creating SendDataHndler. IOException -->" + ex.getMessage());
        }
    }

}
