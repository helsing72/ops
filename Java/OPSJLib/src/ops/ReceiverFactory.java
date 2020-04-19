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
import java.util.logging.Level;
import java.util.logging.Logger;

class ReceiverFactory
{
    static Receiver createReceiver(Topic topic, String localInterface)
    {
        try
        {
            if (topic.getTransport().equals(Topic.TRANSPORT_MC))
            {
                return new MulticastReceiver(topic.getDomainAddress(), topic.getPort(), localInterface, topic.getInSocketBufferSize());
            }
            else if (topic.getTransport().equals(Topic.TRANSPORT_TCP))
            {
                return new TcpClientReceiver(topic.getDomainAddress(), topic.getPort(), topic.getInSocketBufferSize());
            }
            else if (topic.getTransport().equals(Topic.TRANSPORT_UDP))
            {
                if (NetworkSupport.IsMyNodeAddress(topic.getDomainAddress()))
                {
                    // If UDP topic is configured with my node address, we use the configured port, otherwise
                    // we use port 0 which will force the OS to create a unique port that we listen to.
                    return new UDPReceiver(topic.getPort(), topic.getDomainAddress(), topic.getInSocketBufferSize());
                }
                else
                {
                    // We need the system to allocate a port (port == 0 means let system allocate a free one)
                    return new UDPReceiver(0, localInterface, topic.getInSocketBufferSize());
                }
            }
        }
        catch (IOException ex)
        {
            Logger.getLogger(ReceiverFactory.class.getName()).log(Level.SEVERE, null, ex);
        }
        return null;
    }
}
