///////////////////////////////////////////////////////////
//  Domain.cs
//  Implementation of the Class Domain
//  Created on:      12-nov-2011 09:25:30
//  Author:
///////////////////////////////////////////////////////////

using System.Collections.Generic;

namespace Ops 
{
	public class Domain : OPSObject 
    {
		private string domainAddress = "";
		private string domainID = "";
		private string localInterface = "0.0.0.0";
		protected List<Topic> topics = new List<Topic>();
        private int timeToLive = 1;
        private int inSocketBufferSize = 16000000;
        private int outSocketBufferSize = 16000000;
		private int metaDataMcPort = 9494;

        public Domain()
        {
            AppendType("Domain");
        }
        

        public Topic GetTopic(string name)
        {
            foreach (Topic topic in topics)
            {
                if (topic.GetName().Equals(name))
                {
                    if (topic.GetDomainAddress().Equals(""))
                    {
                        topic.SetDomainAddress(domainAddress);
                    }
                    return topic;
                }
            }
            return null;
        }

        public override void Serialize(IArchiverInOut archive)
        {
            // NOTE. Keep this in sync with the C++ version, so it in theory is possible to send these as objects.
            // We need to serialize fields in the same order as C++.
            //OPSObject::serialize(archiver);
            base.Serialize(archive);

            //archiver->inout(std::string("domainID"), domainID);
            //archiver->inout<Topic>(std::string("topics"), topics);
            //archiver->inout(std::string("domainAddress"), domainAddress);
            //archiver->inout(std::string("localInterface"), localInterface);
            domainID = archive.Inout("domainID", domainID);
            topics = (List<Topic>)archive.InoutSerializableList("topics", topics);
            domainAddress = archive.Inout("domainAddress", domainAddress);
            localInterface = archive.Inout("localInterface", localInterface);
            
            //archiver->inout(std::string("timeToLive"), timeToLive);
            //archiver->inout(std::string("inSocketBufferSize"), inSocketBufferSize);
            //archiver->inout(std::string("outSocketBufferSize"), outSocketBufferSize);
            //archiver->inout(std::string("metaDataMcPort"), metaDataMcPort);
            timeToLive = archive.Inout("timeToLive", timeToLive);
    		inSocketBufferSize = archive.Inout("inSocketBufferSize", inSocketBufferSize);
	    	outSocketBufferSize = archive.Inout("outSocketBufferSize", outSocketBufferSize);
            metaDataMcPort = archive.Inout("metaDataMcPort", metaDataMcPort);
        }

        public List<Topic> GetTopics() 
        {
            foreach (Topic topic in topics) 
            {
                if (topic.GetDomainAddress().Equals("")) 
                {
                    topic.SetDomainAddress(domainAddress);
                }
            }
            return topics;
        }

         public bool ExistTopic(string name) 
         {
            foreach (Topic topic in topics) 
            {
                if (topic.GetName().Equals(name)) 
                {
                    return true;
                }
            }
            return false;
        }

        public string GetDomainAddress() 
        {
            return this.domainAddress;
        }

        public string GetDomainID() 
        {
            return this.domainID;
        }

        public string GetLocalInterface() 
        {
            return this.localInterface;
        }

        public int GetMetaDataMcPort()
        {
            return this.metaDataMcPort;
        }

        public int GetInSocketBufferSize()
        {
            return this.inSocketBufferSize;
        }

        public int GetOutSocketBufferSize()
        {
            return this.outSocketBufferSize;
        }

        public int getTimeToLive()
        {
            return this.timeToLive;
        }

        public void SetDomainAddress(string domainAddress) 
        {
            this.domainAddress = domainAddress;
        }

        public void SetDomainID(string domainID) 
        {
            this.domainID = domainID;
        }

        public void SetLocalInterface(string localInterface) 
        {
            this.localInterface = localInterface;
        }

        // If argument contains a "/" we assume it is on the form:  subnet-address/subnet-mask
        // e.g "192.168.10.0/255.255.255.0" or "192.168.10.0/24"
        // In that case we loop over all interfaces and take the first one that matches
        // i.e. the one whos interface address is on the subnet
        public static string DoSubnetTranslation(string ip)
        {
            int index = ip.IndexOf('/');
            if (index < 0) return ip;

            string subnetIp = ip.Substring(0, index);
            string subnetMask = ip.Substring(index + 1);

            byte[] mask;

            if (subnetMask.Length <= 2)
            {
                // Expand to the number of bits given
                int numBits = int.Parse(subnetMask);
                long binMask = (((1 << numBits) - 1) << (32 - numBits)) & 0xFFFFFFFF;

                mask = new byte[4];
                mask[0] = (byte)((binMask >> 24) & 0xFF);
                mask[1] = (byte)((binMask >> 16) & 0xFF);
                mask[2] = (byte)((binMask >>  8) & 0xFF);
                mask[3] = (byte)((binMask      ) & 0xFF);
            }
            else
            {
                mask = System.Net.IPAddress.Parse(subnetMask).GetAddressBytes();
            }

            System.Net.NetworkInformation.IPGlobalProperties computerProperties = System.Net.NetworkInformation.IPGlobalProperties.GetIPGlobalProperties();
            System.Net.NetworkInformation.NetworkInterface[] nics = System.Net.NetworkInformation.NetworkInterface.GetAllNetworkInterfaces();

            if (nics != null && nics.Length > 0)
            {
                foreach (System.Net.NetworkInformation.NetworkInterface adapter in nics)
                {
                    System.Net.NetworkInformation.IPInterfaceProperties properties = adapter.GetIPProperties();
                    System.Net.NetworkInformation.UnicastIPAddressInformationCollection uniCast = properties.UnicastAddresses;
                    if (uniCast == null) continue;

                    foreach (System.Net.NetworkInformation.UnicastIPAddressInformation uni in uniCast)
                    {
                        if (uni.Address.AddressFamily == System.Net.Sockets.AddressFamily.InterNetwork) //IPV4
                        {
                            byte[] addr = uni.Address.GetAddressBytes();
                            for (int j = 0; j < addr.Length; j++) addr[j] = (byte)((int)addr[j] & (int)mask[j]);
                            string Subnet = new System.Net.IPAddress(addr).ToString();

                            if (Subnet.Equals(subnetIp))
                            {
                                return uni.Address.ToString();
                            }
                        }
                    }
                }
            }
            return subnetIp;

            //// This only works on Vista and later
            //System.Net.NetworkInformation.IPGlobalProperties gp = System.Net.NetworkInformation.IPGlobalProperties.GetIPGlobalProperties();
            //System.Net.NetworkInformation.UnicastIPAddressInformationCollection x = gp.GetUnicastAddresses();
            //for (int i = 0; i < x.Count; i++)
            //{
            //    if (x[i].Address.AddressFamily == System.Net.Sockets.AddressFamily.InterNetwork) //IPV4
            //    {
            //        byte[] addr = x[i].Address.GetAddressBytes();
            //        byte[] mask = x[i].IPv4Mask.GetAddressBytes();
            //        for (int j = 0; j < addr.Length; j++) addr[j] = (byte)((int)addr[j] & (int)mask[j]);
            //        string Subnet = new System.Net.IPAddress(addr).ToString();

            //        if (Subnet.Equals(subnetIp))
            //        {
            //            return x[i].Address.ToString();
            //        }
            //    }
            //}
            //return subnetIp;
        }

	}

}