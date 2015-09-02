When data is sent over OPS, it is always packaged in something called OPSMessage. OPSMessage is just like your own data types you define in IDL also an OPSObject. A OPSMessage in turn has another OPSObject field called **data** which is the actual data of the IDL type you defined in your topic configuration.

When you receive data with a subscriber you can choose to access the data directly, or you can access the OPSMessage, which holds some important meta information about the publication.

There are three properties of an OPSMessage of vital importance:

**publicationID** accessed by getter getPublicationID()
> a 64-bit integer value which is increased for each write by a publisher
**publisherName** accessed by getter getPublisherName()
> name of the publisher, set by Publisher.setName()
**topLevelKey** accessed by getter getTopLevelKey()
> the key of the publication. This is the same key value as OPSMessage.getData().getKey() but can be used for filtering purposes etc. without unpacking the data object.


Let's look at an example of how you can read the OPSMessage from a subscriber:

In Java:

```
OPSMessage message = sub.getMessage();
System.out.println("Publication " + message.getPublicationID() + " from " + message.getPublisherName() + " on key = " + message.getTopLevelKey());

```

Or similar in C++:

```
OPSMessage* message = sub.getMessage();
std::cout << "Publication " << message->getPublicationID() << " from " << message->getPublisherName() << " on key = " << message->getTopLevelKey() << std::endl;

```