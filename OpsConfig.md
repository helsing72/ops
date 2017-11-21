# Defining Topics and Configuration #

To make any OPS application work, you need to have an OPS configuration file either on your run directory (assumed in all examples) or point it out explicitly when creating a Participant.

The OPS config file configures the Topics and ties them to data types and transport mechanisms.

Below is a simple such file with only one domain and one topic specified:

```
<?xml version="1.0" encoding="UTF-8"?>
<!--
 Description:
   A template ops_config.xml file, this file is placed in the run directory of all applications that wants
   to use these topics, or pointed out explicitly when creating a Participant.
-->
<root>
    <ops_config type = "DefaultOPSConfigImpl">
        <domains>
            <element type = "Domain">
                <domainID>FooDomain</domainID>
                <domainAddress>234.5.6.8</domainAddress>
                <topics>
                    <element type = "Topic">
                        <name>FooTopic</name>
                        <dataType>foopackage.FooData</dataType>
                        <port>6686</port>
                    </element>
                    <!-- TODO: Add more topics here... -->
                </topics>
            </element>
        </domains>
    </ops_config>
</root>

```

The elements of the configuration files are further described below, note that some are optional and can be omitted.

**ops\_config**, this node contains domains (only type *DefaultOPSConfigImpl* is valid at this point).

**domains**, contains a list of *Domain* implementations (only types *Domain* and *MulticastDomain* (old syntax) is valid at this point).

Elements of _Domain_ contains:
  * **domainID**, which uniquely identifies a domain name in form of a string.
  * **domainAddress**, the multicast ip address which the domain shall use to communicate. Can be overrided per topic but the domains metadata will always use this address.
  * **topics**, a list of *Topic* elements.

Elements of _Topic_ contains:
  * **name**, name of the topic as a string, must be unique within the domain.
  * **dataType**, the data type that samples on this topic must have (samples must be of this type or extend this type).
  * **port**, the ip port that shall be used to communicate on this topic. Only used for some transports mechanisms, see description of *Transport Mechanisms* below.

For more flexibility in the configuration, the elements have several optional tags that can be specified. A description of these follows below.

Optional elements of _Domain_:
  * **localInterface**, defines which local ip interface on which the domain participants shall communicate. If this tag is omitted, the first interface found on the system will be used. If 127.0.0.1 (localinterface) is used, communication will stay on the local machine.
  The interface can be specified with a specific ip address for the machine, e.g. "192.168.10.72", or using a subnet specification like "192.168.10.0/24" or "192.168.10.0/255.255.255.0". Using a subnet specification instead of a specific ip address, makes it possible to have the same configuration on several nodes.
  * **timeToLive**, defines the IP4 *ttl* value to use for multicast communication. This can be used to define how far the communication will reach. If this tag is omitted, a value of 1 is used.
  * **inSocketBufferSize**, sets a default underlying socket buffer size used for topics that doesn't specify its own, see *Topic* below. If this tag is omitted, the OS default is used.
  * **outSocketBufferSize**, sets a default underlying socket buffer size used for topics that doesn't specify its own, see *Topic* below. If this tag is omitted, the OS default is used.
  * **metaDataMcPort**, defines the multicast port used for metadata communication between participants. If this tag is omitted, the default port is 9494. If set to 0, metadata will be disabled. See the *Transport Mechanisms* section below how this effects the communication.
  * **channels**, a list of *Channel* elements, see Channel Configuration below.
  * **transports**, a list of *Transport* elements, see Channel Configuration below.

Optional elements of _Topic_:
  * **sampleMaxSize**, defines the maximum size of the data type when used in this topic. The value is used for reserving memory to be able to buffer data during reception. If this tag is omitted or is specified with a value < 60000, a value of 60000 is used. If a value > 60000 is specified, this topic MUST use its own port, see also [Sending Large Messages](LargeMessages.md).
  * **inSocketBufferSize**, changes the underlying sockets buffer size if possible. If this tag is omitted, the _Domain_ value is used.
  * **outSocketBufferSize**, changes the underlying sockets buffer size if possible. If this tag is omitted, the _Domain_ value is used.
  * **transport**, configures which transport mechanism to be used for this topic. Supported values are *multicast*, *udp* and *tcp*. If tag is omitted, *multicast* is used.
  * **address**, usage depends on the used transport mechanism, see description of *Transport Mechanisms* below.

## Channel Configuration (advanced) ###
In a scenario where a lot of topics are used that use the same transport mechanism, it can be easier to separate the transport information from the topic definition to remove/reduce redundant infomation.

A _Channel_ element defines the transport mechanism, a _Transport_ element defines which topics that use a specific _Channel_ and the _Topic_ element just defines the _name_, _dataType_ and eventually the _sampleMaxSize_. All other values in a _Topic_ element will be replaced with the corresponding _Channel_ value.

Elements of _Channel_ contains:
  * **name**, name of the channel as a string, must be unique within the domain.
  * **linktype**, configures which transport mechanism to be used for this channel. Supported values are *multicast*, *udp* and *tcp*. If tag is omitted, *multicast* is used.
  * **address**, usage depends on the used transport mechanism, see description of *Transport Mechanisms* below.
  * **port**, usage depends on the used transport mechanism, see description of *Transport Mechanisms* below.
  * **localInterface**, see _Domain_ above for a description.
  * **timeToLive**, see _Domain_ above for a description.
  * **inSocketBufferSize**, changes the underlying sockets buffer size if possible. If this tag is omitted, the _Domain_ value is used.
  * **outSocketBufferSize**, changes the underlying sockets buffer size if possible. If this tag is omitted, the _Domain_ value is used.

Elements of _Transport_ contains:
  * **channelID**, name of the channel as a string, must be defined in a _Channel_ element.
  * **topics**, a list of topic names that shall use the channel. Each topic name must be defined in a _Topic_ element.

Note that if a topic specify _sampleMaxSize_ > 60000, it MUST have its own _Channel_. See also [Sending Large Messages](LargeMessages.md).

## Transport Mechanism specifics ##
  * *multicast*: Is a _many-to-many_ transport mechanism. The **port** tag is required. If the **address** tag is specified it overrides the **domainAddress** and lets this topic communicate on its own multicast address.

  For an example see [multicast example](MulticastTransport.md).

  * *udp*: Without specified **address** and **port** tags, OPS uses the metadata sent by participants to connect publishers and subscribers using dynamic ports. This requires metadata to be enabled to work and it is a _many-to-many_ transport mechanism.

  With specified **address** and **port** tags, the metadata is not used and they specify the subscribers address and port. It is a _many-to-one_ transport mechanism.

  For an example see TODO.

  * *tcp*: Is a _one-to-many_ transport mechanism. The **adress** and **port** tags are required and specifies the publishers tcp server address and port to which subscribers connect.

  For an example see [tcp example](TcpTransport.md).

## Tools ##
There is a tool, _VerifyOPSConfig_, that can be used to check the configuration files when they have been edited. The source is in the OPS _Tools_ folder and the built binary in _deploy/bin_ folder.
