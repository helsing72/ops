///////////////////////////////////////////////////////////
//  McUdpSendDataHandler.cs
//  Implementation of the Class McUdpSendDataHandler
//  Created on:      06-jan-2013
//  Author:
///////////////////////////////////////////////////////////

using System.Collections.Generic;
using System.Runtime.CompilerServices;  // Needed for the "MethodImpl" synchronization attribute

namespace Ops 
{
	public class McUdpSendDataHandler : SendDataHandler 
    {
        //                 Topic              port
        private Dictionary<string, Dictionary<string, IpPortPair> > topicSinkMap = new Dictionary<string,Dictionary<string,IpPortPair>>();
        

        public McUdpSendDataHandler(int outSocketBufferSize, string localInterface) 
        {
            sender = new UdpSender(0, localInterface, outSocketBufferSize);  
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public override bool SendData(byte[] bytes, int size, Topic t)
        {
            bool result = true;

            if (this.topicSinkMap.ContainsKey(t.GetName()))
            {
                Dictionary<string, IpPortPair> dict = this.topicSinkMap[t.GetName()];
                List<string> sinksToDelete = new List<string>();

                // Loop over all sinks and send data, remove items that isn't "alive".
                foreach (KeyValuePair<string, IpPortPair> kvp in dict)
                {
                    // Check if this sink is alive
                    if (kvp.Value.IsAlive())
                    {
                        result &= SendData(bytes, size, InetAddress.GetByName(kvp.Value.ip), kvp.Value.port);
                    }
                    else //Remove it.
                    {
                        sinksToDelete.Add(kvp.Key);
                    }
                }
                foreach (string key in sinksToDelete)
                {
                    dict.Remove(key);
                }
            }
            return result;
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public void AddSink(string topic, string ip, int port)
        {
            IpPortPair ipp = new IpPortPair(ip, port);

            if (this.topicSinkMap.ContainsKey(topic))
            {
                Dictionary<string, IpPortPair> dict = this.topicSinkMap[topic];

                if (dict.ContainsKey(ipp.GetKey()))
                {
                    dict[ipp.GetKey()].FeedWatchdog();
                }
                else
                {
                    dict.Add(ipp.GetKey(), ipp);
                }
            }
            else
            {
                Dictionary<string, IpPortPair> dict = new Dictionary<string,IpPortPair>();
                dict.Add(ipp.GetKey(), ipp);
                this.topicSinkMap.Add(topic, dict);
            }
        }

        private class IpPortPair
        {
            public string ip {get; private set; }
            public int port {get; private set; }

            private System.DateTime lastTimeAlive;
            private System.TimeSpan ALIVE_TIMEOUT = System.TimeSpan.FromMilliseconds(3000.0);

            public IpPortPair(string ip, int port)
            {
                this.ip = ip;
                this.port = port;
                this.lastTimeAlive = System.DateTime.Now;
            }

            public bool IsAlive()
            {
                return (System.DateTime.Now - this.lastTimeAlive) < ALIVE_TIMEOUT;
            }

            public void FeedWatchdog()
            {
                this.lastTimeAlive = System.DateTime.Now;
            }

            public string GetKey()
            {
                return port.ToString();
            }
        }
	}

}