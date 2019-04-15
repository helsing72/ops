# UDP Transport #

The UDP Transport of OPS is similar to Multicast Transport, but reduces the nodes that the traffic is sent to, which may reduce the load on the nodes not interrested in the data. The drawback is that the publisher need to send the data several times, once for each receiving node.

The UDP transport can be configured in two different ways:

Without specified **address** and **port** tags, OPS uses the metadata sent by participants to connect publishers and subscribers using dynamic ports. This requires metadata to be enabled to work and it is a _many-to-many_ transport mechanism.

With specified **address** and **port** tags, the metadata is not used and they specify the subscribers address and port. It is a _many-to-one_ transport mechanism.

To use the UDP transport for a topic, this is how to setup your [Topic Config](OpsConfig.md) file:

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
                        <!-- Required for UDP Transport-->
                        <name>FooTopic</name>
                        <dataType>foopackage.FooData</dataType>
                        <transport>udp</transport>

                        <!-- Optional config -->
                        <sampleMaxSize>60000</sampleMaxSize>
                        <inSocketBufferSize>1000000</inSocketBufferSize>
                        <outSocketBufferSize>1000000</outSocketBufferSize>
                    </element>
                    <element type = "Topic">
                        <!-- UDP to a specified receiver -->
                        <name>BarTopic</name>
                        <dataType>barpackage.BarData</dataType>
                        <transport>udp</transport>
                        <address>192.168.73.11</address>
                        <port>7777</port>
                    </element>
                    <!-- TODO: Add more topics here... -->
                </topics>
            </element>
        </domains>
    </ops_config>
</root>

```
As you can see, the field transport must be set to "udp". If address and port is specified it is the ip address of the machine running the subscriber for the topic (a many-to-one communication).
Other fields has the same impact as for the [Multicast Transport](MulticastTransport.md).

See also, [Topic Config](OpsConfig.md).
