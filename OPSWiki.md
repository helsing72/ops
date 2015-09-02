# Welcome to the OPS Wiki #

## Introduction ##

OPS primarily targets distributed real-time and embedded (DRE) systems and distributed control systems (DCS).

OPS aims to offer asynchronous, efficient and type secure publish-subscribe communication between processes running on multiple platforms written in multiple programming languages.

The communication model of OPS is a typed topic publish-subscribe model. That means that processes that take part in the OPS communication can act as publishers or subscribers to different topics. Data published on a certain topic will be delivered to all subscribers to that topic in a way that hides the publishers from the subscribers and vice versa. By ensuring this anonymity the middleware allows for low coupling between different system participants which enables software reuse, modularity and redundancy among other desirable properties.

As mentioned above, OPS offers typed topics, which in addition to connecting publishers and subscribers also dictates the format of the data that is sent between them. The messages sent in OPS are Data classes that are defined by the system developers in a data definition language called OPS IDL. These data classes may be complex constructs, with inheritence, dynamic arrays, strings, classes of classes and arrays of classes. These Data classes are used to auto generate source code, for different target programming languages, that allows for communicating this data in a simple and secure way. The complexity of serializing and deserializing these classes and to be able to send them across the network is completely hidden from the participants, and the classes can be used in an object oriented and type safe way in the application layer of the participants.

Presently OPS is available in **Java** (Windows, Linux, Unix, Mac) and **C++** (Win32) programming languages. OPS can also be used under **Matlab** through the Java Matlab bridge available with Matlab. Implemetations for C++ (Linux/GCC) and C# is planned and are being developed.

### Related work ###
The idea of OPS is not unique, it is highly inspired by OMG's [DDS](http://portals.omg.org/dds) (Data Distribution Service) specification both in terms of functionality, API's, ideas and terminology. What OPS offers is an easier to learn and use variant than the DDS specification and its implementations available. Moreover, by using simple threading models and protocol, the OPS core is easy to understand both in terms of learning to use it, but also to understand what consequences it has on the systems that use it. By offering plugin mechanisms, OPS still ensures that complicated distributed systems with unique demands can be targeted.


## New to Publish/Subscribe? ##
> Here are some resources to get you started:
  * http://en.wikipedia.org/wiki/Publish/subscribe
  * http://www.cse.wustl.edu/~corsaro/papers/pubsubChapter.pdf

## Acknowlegments ##
> OPS makes heavy use of the following other Open Source Projects:
  * http://www.netbeans.org , the tools for OPS are built on Netbeans Platform.
  * http://www.boost.org , The C++ implementation of OPS use several boost libraries to keep platform independant. Most notable, boost.asio.

## Getting started with OPS ##
  * [UserGuide](UserGuide.md)