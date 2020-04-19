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
import java.net.Inet4Address;
import java.net.InetAddress;
import java.util.Vector;
import java.util.HashMap;

public class McUdpSendDataHandler extends SendDataHandlerBase
{
    private class Entry_T
    {
        public boolean staticRoute = false;
        public HashMap<String, IpPortPair> portMap = new HashMap<String, IpPortPair>();
        public Entry_T(boolean staticRoute) { this.staticRoute = staticRoute; }
    }

    //              Topic
    private HashMap<String, Entry_T> topicSinkMap = new HashMap<String,Entry_T>();

    public McUdpSendDataHandler(int outSocketBufferSize, String localInterface) throws IOException
    {
        sender = new UDPSender(0, localInterface, outSocketBufferSize);
    }

    public synchronized boolean sendData(byte[] bytes, int size, Topic t)
    {
        boolean result = true;

        if (this.topicSinkMap.containsKey(t.getName()))
        {
            Entry_T dict = this.topicSinkMap.get(t.getName());
            Vector<String> sinksToDelete = new Vector<String>();

            // Loop over all sinks and send data, remove items that isn't "alive".
            for (IpPortPair ipp : dict.portMap.values())
            {
                // Check if this sink is alive
                if (ipp.IsAlive())
                {
                    result &= sendData(bytes, size, ipp.ip, ipp.port);
                }
                else //Remove it.
                {
                    sinksToDelete.add(ipp.key);
                }
            }
            for (String key : sinksToDelete)
            {
                dict.portMap.remove(key);
            }
        }
        return result;
    }

    public synchronized void addSink(String topic, String ip, int port, boolean staticRoute) throws IOException
    {
        IpPortPair ipp = new IpPortPair(InetAddress.getByName(ip), port, staticRoute);

        if (this.topicSinkMap.containsKey(topic))
        {
            Entry_T dict = this.topicSinkMap.get(topic);

            //If created as static route, we only add sinks that are static
            if ((!dict.staticRoute) || (dict.staticRoute && staticRoute))
            {
                if (dict.portMap.containsKey(ipp.key))
                {
                    dict.portMap.get(ipp.key).FeedWatchdog(staticRoute);
                }
                else
                {
                    dict.portMap.put(ipp.key, ipp);
                }
            }
        }
        else
        {
            Entry_T dict = new Entry_T(staticRoute);
            dict.portMap.put(ipp.key, ipp);
            this.topicSinkMap.put(topic, dict);
        }
    }

    private class IpPortPair
    {
        public InetAddress ip;
        public int port;
        public String key;

        private boolean alwaysAlive = false;
        private long lastTimeAlive;
        private long ALIVE_TIMEOUT = 3000;  // [ms]

        public IpPortPair(InetAddress ip, int port, boolean alwaysAlive)
        {
            this.ip = ip;
            this.port = port;
            this.key = ip.toString() + ":" + port;
            this.alwaysAlive = alwaysAlive;
            this.lastTimeAlive = System.currentTimeMillis();
        }

        public boolean IsAlive()
        {
            return this.alwaysAlive || ((System.currentTimeMillis() - this.lastTimeAlive) < ALIVE_TIMEOUT);
        }

        public void FeedWatchdog(boolean alwaysAlive)
        {
            this.alwaysAlive |= alwaysAlive;
            this.lastTimeAlive = System.currentTimeMillis();
        }
    }

}
