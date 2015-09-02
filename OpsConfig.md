# Defining Topics and Configuration #

To make any OPS application work, you need to have a OPS configuration file either on your run directory (assumed in all examples) or point it out specifically when creating a Participant (not fully supported yet).

The OPS config file configures the Topics and ties them to data types and transport mechanisms.

Below is a simple such file with only one domain and one topic specified:

```
<?xml version="1.0" encoding="UTF-8"?>
<!--
 Description:
 A template ops_config.xml file, this file shall be put on run directory of all applications that wants to use these topics.
-->
<root>
    <ops_config type = "DefaultOPSConfigImpl">
        <domains>
            <element type = "MulticastDomain">
                <domainID>FooDomain</domainID>
                <domainAddress>234.5.6.8</domainAddress>
                <localInterface>127.0.0.1</localInterface>
                <topics>
                    
                    <element type = "Topic">
                        <name>FooTopic</name>
                        <port>6686</port>
                        <dataType>foopackage.FooData</dataType>
                        <sampleMaxSize>60000</sampleMaxSize>
                        <inSocketBufferSize>1000000</inSocketBufferSize>
                        <outSocketBufferSize>1000000</outSocketBufferSize>
                        <transport>multicast</transport>
                        <address>234.5.6.9</address>
                    </element>
                    <!-- TODO: Add more topics here... -->
                </topics>
            </element>

        </domains>
    </ops_config>
</root>

```

The elements of the configuration files are futher described below, note that some are optional and can be omitted.

**ops\_config**, this node contains a list of Domains called domains (only ! DefaultOPSConfigImpl is valid at this point).

**domains**, contains a list of Domain implementations (only MulticastDomain is valid at this point)

elements of _MulticastDomain_ contains:
  * **domainID**, which uniquely identifies a domain name in form of a string
  * **domainAddress**, a multicast ip address which the domain shall use to communicate. domainAddress must be the same for all domain with the same domainID
  * **localInterface** (optional) defines which local ip interface on which the domain participants shall communicate. If this tag is omitted, the first interface found on the system will be used. Especially if 127.0.0.1 (localinterface) is used communication will stay on the local machine only.
  * **topics**, a list of Topic

elements of _Topic_ contains:
  * **name**, name of the topic as a string, shall be unique within the domain
  * **port**, the port that should be used to communicate on this topic
  * **dataType**, the parent data type that samples on this topic must be (samples must be of this type or extend this type)
  * **inSocketBufferSize** (optional), changes the underlying sockets buffer size if possible
  * **outSocketBufferSize** (optional) changes the underlying sockets buffer size if possible
  * **transport** (optional), configures which transport to be used for this topic. multicast is default and the only transport fully supported. Others include tcp and udp which exist as experimental implementations.
  * **address** (optional), overrides the domainAddress and lets this topic comunicate on its own multicast address