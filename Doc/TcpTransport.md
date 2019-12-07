# TCP Transport #


The TCP Transport of OPS is based on TCP/IP as opposed to Multicast Transport which use UDP as the basis for communication. When you use TCP as the transport for a topic, OPS sets up a server on the publisher side of a topic which accepts connections from subscribers and sends data to the subscribers one by one over TCP when a write operation is called on the publisher.

The TCP transport can be useful in network environments with high IP package losses and/or for communicating over the Internet or other complex networks where routers may block multicast traffic.

The TCP transport can be configured in two different ways:

Without specified **address** and **port** tags, OPS uses the metadata sent by participants to connect publishers and subscribers using dynamic ports. This requires metadata to be enabled to work and it is a _many-to-many_ transport mechanism.

With specified **address** and **port** tags, the metadata is not used and they specify the publishers address and port. It is a _one-to-many_ transport mechanism.

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
            <element type = "Domain">
                <domainID>FooDomain</domainID>
                <domainAddress>234.5.6.8</domainAddress>
                <localInterface>127.0.0.1</localInterface>
                <topics>
                    <element type = "Topic">
                        <!-- Required for TCP Transport with a specified sender -->
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
                    <element type = "Topic">
                        <!-- Required for TCP Transport using many-to-many -->
                        <name>BarTopic</name>
                        <dataType>foopackage.FooData</dataType>
                        <transport>tcp</transport>
                    </element>
                    <!-- TODO: Add more topics here... -->
                </topics>
            </element>
        </domains>
    </ops_config>
</root>

```

As you can see, the field transport must be set to "tcp". If address and port is specified it is the ip address of the machine running the publisher for the topic (a one-to-many communication).
Other fields has the same impact as for the [Multicast Transport](MulticastTransport.md).

The TCP transport protocol exists in two versions, where v2 added heartbeats to enable faster detection of a broken link, see [protocol description](Protocol.md)). Currently the v2 protocol only exist in C++ and Ada implementations.

See also, [Topic Config](OpsConfig.md).
