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
import java.util.HashMap;

class SendDataHandlerFactory
{
    private HashMap<String, SendDataHandler> sendDataHandlers = new HashMap<String, SendDataHandler>();

    private McUdpSendDataHandler udpSendDataHandler = null;

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
            String localIf = NetworkSupport.DoSubnetTranslation(t.getLocalInterface());

            if (t.getTransport().equals(Topic.TRANSPORT_MC))
            {
                sender = new McSendDataHandler(t, localIf, t.getTimeToLive());
            }
            if (t.getTransport().equals(Topic.TRANSPORT_TCP))
            {
                sender = new TcpSendDataHandler(t, localIf);
            }
            if (sender != null)
            {
                sendDataHandlers.put(key, sender);
                return sender;
            }

            // We don't want to add the udp handler to the handler list, so handle this last
            if (t.getTransport().equals(Topic.TRANSPORT_UDP))
            {
                if (udpSendDataHandler == null)
                {
                    // We have only one sender for all topics, so use the domain value for buffer size
                    udpSendDataHandler = new McUdpSendDataHandler(
                        participant.getDomain().GetOutSocketBufferSize(),
                        localIf);
                }

                if (NetworkSupport.IsValidNodeAddress(t.getDomainAddress()))
                {
                    udpSendDataHandler.addSink(t.getName(), t.getDomainAddress(), t.getPort(), true);
                }
                else
                {
                    // Setup a listener on the participant info data published by participants on our domain.
                    // We use the information for topics with UDP as transport, to know the destination for UDP sends
                    // ie. we extract ip and port from the information and add it to our McUdpSendDataHandler
                    participant.connectUdp(udpSendDataHandler);
                }
                return udpSendDataHandler;
            }

            throw new CommException("No such Transport " + t.getTransport());
        }
        catch (IOException ex)
        {
            throw new CommException("Error creating SendDataHandler. IOException --> " + ex.getMessage());
        }
    }

}
