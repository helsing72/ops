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
        private int inSocketBufferSize = -1;    // Use OS default
        private int outSocketBufferSize = -1;   // Use OS default
		private int metaDataMcPort = 9494;

        private List<Channel> channels = new List<Channel>();
        private List<Transport> transports = new List<Transport>();

        public Domain()
        {
            AppendType("Domain");
        }

        void checkTopicValues(Topic top)
        {
            if (top.GetDomainAddress().Equals(""))
            {
                top.SetDomainAddress(domainAddress);
            }
            if (top.GetLocalInterface().Equals(""))
            {
                top.SetLocalInterface(localInterface);
            }
            if (top.GetTimeToLive() < 0)
            {
                top.SetTimeToLive(timeToLive);
            }
            if (top.GetInSocketBufferSize() < 0)
            {
                top.SetInSocketBufferSize(inSocketBufferSize);
            }
            if (top.GetOutSocketBufferSize() < 0)
            {
                top.SetOutSocketBufferSize(outSocketBufferSize);
            }
        }

        public Topic GetTopic(string name)
        {
            foreach (Topic topic in topics)
            {
                if (topic.GetName().Equals(name))
                {
                    checkTopicValues(topic);
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

        	// To not break binary compatibility we only do this when we know we are
        	// reading from an XML-file
            if (archive is XMLArchiverIn) { 
        		//archiver->inout<Channel>(std::string("channels"), channels);
		        //archiver->inout<Transport>(std::string("transports"), transports);
        		channels = (List<Channel>)archive.InoutSerializableList("channels", channels);
                transports = (List<Transport>)archive.InoutSerializableList("transports", transports);
        		checkTransports();
        	}
        }

        private Channel findChannel(string id)
        {
        	if (!id.Equals("")) {
		        for (int i = 0; i < channels.Count; i++) {
        			if (id.Equals(channels[i].channelID)) return channels[i];
		        }
        	}
    	    return null;
        }

        private Topic findTopic(string id)
        {
        	if (!id.Equals("")) {
        		for (int i = 0; i < topics.Count; i++) {
		        	if (id.Equals(topics[i].GetName())) return topics[i];
        		}
        	}
        	return null;
        }

        void checkTransports()
        {
        	// Now update topics with values from the transports and channels
        	// Loop over all transports and for each topic, see if it needs parameters from the channel
        	for (int i = 0; i < transports.Count; i++) {
        		// Get channel
        		Channel channel = findChannel(transports[i].channelID);
        		if (channel == null) {
        			throw new ConfigException(
		        		"Non existing channelID: '" + transports[i].channelID +
				        "' used in transport spcification.");
        		} else {
		        	for (int j = 0; j < transports[i].topics.Count; j++) {
				        Topic top = findTopic(transports[i].topics[j]);
    	    			if (top == null) {
	    	    			throw new ConfigException(
				    	    	"Non existing topicID: '" + transports[i].topics[j] +
					    	    "' used in transport spcification.");
        				} else {
		        			channel.populateTopic(top);
				        }
        			}
		        }
        	}
        }

        public List<Topic> GetTopics() 
        {
            foreach (Topic topic in topics) 
            {
                checkTopicValues(topic);
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

            byte[] bip = System.Net.IPAddress.Parse(subnetIp).GetAddressBytes();
            byte[] bmask;

            if (subnetMask.Length <= 2)
            {
                // Expand to the number of bits given
                int numBits = int.Parse(subnetMask);
                long binMask = (((1 << numBits) - 1) << (32 - numBits)) & 0xFFFFFFFF;

                bmask = new byte[4];
                bmask[0] = (byte)((binMask >> 24) & 0xFF);
                bmask[1] = (byte)((binMask >> 16) & 0xFF);
                bmask[2] = (byte)((binMask >>  8) & 0xFF);
                bmask[3] = (byte)((binMask      ) & 0xFF);
            }
            else
            {
                bmask = System.Net.IPAddress.Parse(subnetMask).GetAddressBytes();
            }

            for (int j = 0; j < bip.Length; j++) bip[j] = (byte)((int)bip[j] & (int)bmask[j]);

//            System.Net.NetworkInformation.IPGlobalProperties computerProperties = System.Net.NetworkInformation.IPGlobalProperties.GetIPGlobalProperties();
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
                            for (int j = 0; j < addr.Length; j++) addr[j] = (byte)((int)addr[j] & (int)bmask[j]);

                            bool eq = true;
                            for (int j = 0; j < addr.Length; j++) eq = eq & (addr[j] == bip[j]);

                            if (eq)
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