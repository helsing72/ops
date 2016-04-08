# Publishing On Topics #

Given a IDL project FooProject with a data type FooData used by a topic FooTopic on a domain FooDomain this is how you set up publishers and write data:


## Java ##


```

//Get a participant reference, this is your entry point to OPS
Participant participant = Participant.getInstance("FooDomain");
if(participant == null)
{
    //Report error
    return;
}
//Add suppoert for serializing/unserialazing our data types
participant.addTypeSupport(new FooProject.FooProjectTypeFactory());

Topic topic = participant.createTopic("FooTopic");

FooDataPublisher pub = new FooDataPublisher(topic);

//Create some data
FooData data = new FooData();

//Publish the data
pub.write(data);


```



## C++ ##

```
//Get a participant reference, this is your entry point to OPS
Participant* participant = Participant::getInstance("FooDomain");
if(participant == NULL)
{
    //Report error
    return;
}
//Add suppoert for serializing/unserialazing our data types
participant->addTypeSupport(new FooProject::FooProjectTypeFactory());


Topic topic = participant->createTopic("FooTopic");

FooDataPublisher pub(topic);

//Create some data
FooData data;

//Publish the data
pub.write(&data);

```