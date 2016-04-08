When setting up a subscription you can set a deadline timeout which tells the OPS core at which rate you expects to get data on a certain topic. If this deadline is not met, i.e. no sample is published within your deadline, you can get notified by an event or by having method isDeadlineMissed() return true.

This is how you do it, examplified in C++ by polling:

```
//Get a participant reference, this is your entry point to OPS
Participant* participant = Participant::getInstance("FooDomain");
if(participant == NULL)
{
    //Report error
    return;
}
//Add support for serializing/unserialazing our data types
participant->addTypeSupport(new FooProject::FooProjectTypeFactory());

Topic topic = participant->createTopic("FooTopic");

FooDataSubscriber sub(topic);

sub.setDedlineQoS(1000); //In milliseconds

sub.start();

//Check if deadline is missed
if(sub.isDeadlineMissed())
{
    //do something about it
}

```

or you can do it by regisering a listener like this:

```

class MyDeadlineListener : public ops::DeadlineMissedListener
{
   MyListener(FooDataSubscriber* s)
   {
       s->deadlineMissedEvent.addDeadlineMissedListener(this);
   } 
   ///Override from ops::DeadlineMissedListener
   void onDeadlineMissed(ops::DeadlineMissedEvent*)
   {
       //React on deadline missed
    
   } 

}
```

and in addition to your init code above register this listener with the subscriber

```
MyDeadlineListener dListener(&sub);

```

In Java we have the same approach but by using Observer:

```
//Adding a deadline Observer inline
sub.deadlineEvent.addObserver(new Observer() 
{
   public void update(Observable o, Object arg)
   {
      //React on deadline missed
   }
});

```

Or again, of course just:

```
if(sub.isDeadlineMissed())
{
   //React!
}
```

You can change the value of the Deadline QoS at any time.