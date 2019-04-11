# OPS Bridge #
***OPSBridge*** is a tool for replicating UDP/MC messages in one node, into another node (possibly accross the internet), using a TCP/IP channel between the nodes.

Start an instance of OPSBridge on each of the two nodes that need to be connected, one side is a TCP server and the other side is a TCP client. A configuration file defines which UDP/MC ports to replicate between the nodes.

The following shows the available command line arguments:

```
Usage: OPSBridge <commands>

  -b n            Bridge to use from configuration file (sequence number) [0]
  -n name         Named bridge to use from configuration file []
  -c cfgfile      OPSBridge configuration file to use [opsbridge_config.xml]
  -l n            Log level to use 0..9 (higher value --> less logging) [0]
  -h | -?         Show this help
```

Example of a configuration file:
```
<?xml version="1.0" encoding="UTF-8"?>
<!--
    Description:
        Configuration parameters for the OPSBridge program.
-->
<root>
  <logging config="true" debug="false"/>

  <!--  Define the bridges that should be used -->
  <bridges>
    <!--  Define how this bridge should work:
          name			  	Will be used as name of the bridge
    -->
    <bridge name="Bridge1">
      <!--  Define communication endpoint
            tcpClient			  This side is a TCP Client
              remoteHost		IP address to connect to
              remotePort		IP Port to connect to
            tcpServer			  This side is a TCP Server
              localPort		  IP Port to listen on
      -->
      <endpoint type="tcpclient" remoteHost="192.168.0.33" remotePort="20202"/>

      <example_endpoint type="tcpclient" remoteHost="127.0.0.1" remotePort="20202"/>
      <example_endpoint type="tcpserver" localPort="23232"/>

      <!-- Define Raw UDP and Multicast (MC) packet-bridging.

        Received UDP and MC packets are transfered to the communication endpoint using the choosen transport mechanism.
        As default, UDP packets will be resent using address "127.0.0.1" and the receive port.
        As default, MC packets will be resent using the receive MC address and port.
        If the address and/or port for a send need to be changed, the translation can be specified on the sending side.

        receive:  
          ip        MC / UDP address that the bridge should listen to.
          port      MC / UDP port that the bridge should listen to
          if        MC, interface to receive on (default "127.0.0.1")

        send:    Specified if a translation need to be done (ie. need to change defaults)
          ip        MC / UDP address used in receive
          port      MC / UDP port used in receive
          newip     MC / UDP address used for sending (UDP default "127.0.0.1", MC default same as receive address)
          newport   MC / UDP port used for sending (default same as receive port)
          if        MC, interface to send on (default "127.0.0.1")
          ttl       MC, ttl to use (default 1)
      -->
      <raw>
        <receive ip="236.12.13.14" port="6680"/>

        <example_receive ip="236.4.5.6" port="12345" if="192.168.0.26"/>
        <example_receive ip="127.0.0.1" port="32322"/>
        <example_send ip="236.1.2.3" port="22222" newip="239.5.6.7" newport="23456" if="127.0.0.1" ttl="1"/>
        <example_send ip="127.0.0.1" port="32322" newip="" newport="32444"/>
      </raw>
    </bridge>

    <bridge name="Bridge2">
      <endpoint type="tcpserver" localPort="20202"/>

      <raw>
        <receive ip="236.12.13.14" port="6680"/>
      </raw>
    </bridge>

  </bridges>

</root>
```
The setup above, where both sides specify the same raw receive MC port and no translation of addresses or ports, result in all MC trafic on the specified port in each node to be replicated to the other node. OPSBridge has means to block its own sent messages to not create a feedback loop.

Examples of usage with the configuration file above:

On node 1. Start bridge as a TCP Client ("Bridge1" in the example file)
```
./OPSBridge -c opsbridge_config.xml -b 0
```
On node 2. Start bridge as a TCP Server ("Bridge2" in the example file)
```
./OPSBridge -c opsbridge_config.xml -n Bridge2
```

The source for the tool is in the OPS _Tools_ folder and the built binary in the _deploy/bin_ folder.
