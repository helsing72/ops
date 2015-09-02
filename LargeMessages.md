# Sending Large Messages with OPS #


---

**Note**, with topics that specify a sampleMaxSize > 60000 there can only be one publisher publishing at the same time. If two publishers write at the same time, there is a risk that messages will be lost and not delivered to the subscribers as expected.

**Note also**, this feature requires the Topic to use its own port (See OpsConfig).

**Note also**, this feature is NOT fully supported by the Java implementation of OPS.

---


OPS is by default using a UDP multicast based transport (see MulticastTransport). UDP is a message based technology that fits well with the communication model of OPS. UDP does however put some restriction on the communication including the fact that UDP messages have a limited size. What this exact size is varies from platform to platform but a common size limit is 65535 bytes. Because of this, OPS has a default size limit for messages at 60000 bytes of user data in an OPS message. OPS allows you to increase this size limit if desirable.

This is how to configure a topic on which you wish to send data samples larger than the default 60000 byte limit:

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
                        <name>LargeSampleTopic</name>
                        <port>6686</port>
                        <dataType>foopackage.FooData</dataType>
                        <sampleMaxSize>5000000</sampleMaxSize>
                        <inSocketBufferSize>10000000</inSocketBufferSize>
                        <outSocketBufferSize > 10000000</outSocketBufferSize>
                        
                    </element>
                    <!-- TODO: Add more topics here... -->
                </topics>
            </element>

        </domains>
    </ops_config>
</root>

```

In the sample above, we have configured the topic to allow passing messages of a maximum size of 5 MB. Note also that the inSocketBufferSize and outSocketBufferSize have been adjusted to 10 MB (the exact value of the buffer sizes should depend on the rate of the topic and size of the messages, this is just an estimate).

The configuration above is all that is necessary to send large messages.

**Note however that at the moment doing so put limitation of the use of the topic. With topics that specify a sampleMaxSize > 60000 there can only be one publisher publishing at the same time. If two publishers write at the same time, there is a risk that messages will be lost and not delivered to the subscribers as expected.**