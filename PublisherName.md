# Publisher Name #

Even though the anonymous principle of OPS is often desirable, there are cases where which publisher that published a certain sample can be of interest.

For such cases OPS has a meta field sent with all [OPSMessages](OpsMessage.md) called publisherName. This field can be used for identifying what publisher published a certain sample. This is also a useful feature for debugging (see [OpsDebugger](OpsDebugger.md)) even if the publisherName remains anonymous from a application perspective.

To set the publisherName, use:

` publisher.setName("my_pub_name"); `

and to read it from a received sample (OPSMessage) use,

`string name = subscriber.getMessage()->getPublisherName(); `

To date, there exist no PublisherNameFilterQos like for keys, but it is easy to implement one of your own if you need it, see [Custom Filter QoS Policies](PluginFilterQoS.md).