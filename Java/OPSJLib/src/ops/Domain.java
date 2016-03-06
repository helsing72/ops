/**
*
* Copyright (C) 2006-2009 Anton Gravestam.
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

import configlib.ArchiverInOut;
import java.io.IOException;
import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.NetworkInterface;
import java.util.Enumeration;
import java.util.Vector;

/**
 *
 * @author angr
 */
public class Domain extends OPSObject
{
    private String domainAddress = "";
    private String domainID = "";
    private String localInterface = "0.0.0.0";
    protected Vector<Topic> topics = new Vector<Topic>();
    private int timeToLive = 1;
    private int inSocketBufferSize = 16000000;
    private int outSocketBufferSize = 16000000;
    private int metaDataMcPort = 9494;


    public Domain()
    {
        appendType("Domain");
    }


    public Topic getTopic(String name)
    {
        for (Topic topic : topics)
        {
            if(topic.getName().equals(name))
            {
                if(topic.getDomainAddress().equals(""))
                {
                    topic.setDomainAddress(domainAddress);
                }
                return topic;
            }
        }
        return null;
    }

    @Override
    public void serialize(ArchiverInOut archive) throws IOException
    {
        // NOTE. Keep this in sync with the C++ version, so it in theory is possible to send these as objects.
        // We need to serialize fields in the same order as C++.
        //OPSObject::serialize(archiver);
        super.serialize(archive);

        //archiver->inout(std::string("domainID"), domainID);
        //archiver->inout<Topic>(std::string("topics"), topics);
        //archiver->inout(std::string("domainAddress"), domainAddress);
        //archiver->inout(std::string("localInterface"), localInterface);
        domainID = archive.inout("domainID", domainID);
        topics = (Vector<Topic>) archive.inoutSerializableList("topics", topics);
        domainAddress = archive.inout("domainAddress", domainAddress);
        localInterface = archive.inout("localInterface", localInterface);

        //archiver->inout(std::string("timeToLive"), timeToLive);
        //archiver->inout(std::string("inSocketBufferSize"), inSocketBufferSize);
        //archiver->inout(std::string("outSocketBufferSize"), outSocketBufferSize);
        //archiver->inout(std::string("metaDataMcPort"), metaDataMcPort);
        timeToLive = archive.inout("timeToLive", timeToLive);
	inSocketBufferSize = archive.inout("inSocketBufferSize", inSocketBufferSize);
    	outSocketBufferSize = archive.inout("outSocketBufferSize", outSocketBufferSize);
        metaDataMcPort = archive.inout("metaDataMcPort", metaDataMcPort);
    }


    public Vector<Topic> getTopics() {
        for (Topic topic : topics) {
            if (topic.getDomainAddress().equals("")) {
                topic.setDomainAddress(domainAddress);
            }
        }
        return topics;
    }

     public boolean existTopic(String name) {
        for (Topic topic : topics) {
            if (topic.getName().equals(name)) {
                return true;
            }
        }
        return false;
    }

    public String getDomainAddress() {
        return domainAddress;
    }

    public String getDomainID() {
        return domainID;
    }

    public String getLocalInterface() {
        return localInterface;
    }

    public int GetMetaDataMcPort()
    {
        return metaDataMcPort;
    }

    public int GetInSocketBufferSize()
    {
        return inSocketBufferSize;
    }

    public int GetOutSocketBufferSize()
    {
        return outSocketBufferSize;
    }

    public int getTimeToLive()
    {
        return timeToLive;
    }

    public void setDomainAddress(String domainAddress) {
        this.domainAddress = domainAddress;
    }

    public void setDomainID(String domainID) {
        this.domainID = domainID;
    }

    public void setLocalInterface(String localInterface) {
        this.localInterface = localInterface;
    }

    // If argument contains a "/" we assume it is on the form:  subnet-address/subnet-mask
    // e.g "192.168.10.0/255.255.255.0" or "192.168.10.0/24"
    // In that case we loop over all interfaces and take the first one that matches
    // i.e. the one whos interface address is on the subnet
    public static String DoSubnetTranslation(String ip)
    {
        int index = ip.indexOf('/');
        if (index < 0) return ip;

        String subnetIp = ip.substring(0, index);
        String subnetMask = ip.substring(index + 1);

        try
        {
            byte[] mask;
            if (subnetMask.length() <= 2) {
		            // Expand to the number of bits given
                long bitmask = Integer.parseInt(subnetMask);
		            bitmask = (((1 << bitmask)-1) << (32 - bitmask)) & 0xFFFFFFFF;
                mask = new byte[] {
                  (byte)(bitmask >>> 24),
                  (byte)(bitmask >>> 16),
                  (byte)(bitmask >>> 8),
                  (byte)bitmask};
            } else {
                InetAddress ipAddress = InetAddress.getByName(subnetMask);
                mask = ipAddress.getAddress();
            }

            Enumeration<NetworkInterface> nets = NetworkInterface.getNetworkInterfaces();
            for (NetworkInterface netint : java.util.Collections.list(nets))
            {
                java.util.List<java.net.InterfaceAddress> ifAddresses = netint.getInterfaceAddresses();
                for (java.net.InterfaceAddress ifAddress : ifAddresses) {
                    if (ifAddress.getAddress() instanceof Inet4Address) {
                        byte[] addr = ifAddress.getAddress().getAddress();
                        for (int j = 0; j < addr.length; j++) addr[j] = (byte)((int)addr[j] & (int)mask[j]);

                        String Subnet = InetAddress.getByAddress(addr).toString();
                        index = Subnet.indexOf('/');
                        if (index >= 0) Subnet = Subnet.substring(index+1);

                        if (Subnet.equals(subnetIp))
                        {
                            // split "hostname/127.0.0.1/8 [0.255.255.255]"
                            String s[] = ifAddress.toString().split("/");
                            return s[1];
                        }
                    }
                }
            }
        }
        catch (Exception e)
        {
        }
        return subnetIp;
    }

}
