# TCP Transport #


---

Note, this transport only exist for C++ in an experimental implementation and is under development.

---


The TCP Transport of OPS is based on TCP/IP as opposed to Muticast Transport which use UDP as the basis for communication. When you use TCP as the transport for a topic, OPS setts up a server on the publisher side of a topic which accpets connections from subscribers and sends data to the subscribers one by one over TCP when a write operation i called on the publisher.

The TCP transport can be useful in network environments with high IP package losses and/or for communicating over the Internet.

When using TCP Transport, a topic can only have one publisher but any number of subscribers to a topic.

To use the TCP transport for a topic, this is how to setup your [Topic Config](OpsConfig.md) file:

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

                        <!-- Required for TCP Transport-->
                        <name>FooTopic</name>
                        <port>6686</port>
                        <dataType>foopackage.FooData</dataType>
                        <address>127.0.0.1</address>
                        <transport>tcp</transport>

                        <!-- Optional config -->
                        <sampleMaxSize>60000</sampleMaxSize>
                        <inSocketBufferSize>1000000</inSocketBufferSize>
                        <outSocketBufferSize>1000000</outSocketBufferSize>
   
                    </element>
                    <!-- TODO: Add more topics here... -->
                </topics>
            </element>

        </domains>
    </ops_config>
</root>

```

As you can see, the field address and transport must be specified. The field transport must be set to "tcp" and address is the ip address of the machine running the publisher for the topic (once again note that the TCP Transport only support one-to-many communication).
Other fields has the same impact as for the [Multicast Transport](MulticastTransport.md).