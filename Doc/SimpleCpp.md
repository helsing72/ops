[Installation](Installation.md) [Examples](SimpleCpp.md) [BuildInstructions](BuildInstructions.md)

---





> 

&lt;meta http-equiv="content-type" content="text/html; charset=utf-8"/&gt;


> 

&lt;title&gt;

Google Wave Embed API Example: Simple Wave

&lt;/title&gt;


> 

&lt;script src="http://wave-api.appspot.com/public/embed.js" type="text/javascript"&gt;



&lt;/script&gt;


> 

&lt;script type="text/javascript"&gt;


> function initialize() {
> > var wavePanel = new WavePanel('http://wave.google.com/a/wavesandbox.com/');
> > wavePanel.loadWave('wavesandbox.com!w+waveID');
> > wavePanel.init(document.getElementById('waveframe'));

> }
> 

&lt;/script&gt;



> 

&lt;body onload="initialize()"&gt;


> > <div></div>

> 

&lt;/body&gt;





# Basic C++ Example #

Given the following type defined in [OPS IDL](IDLCompilerTutorial.md):

```

package foo;

class FooData
{
    string fooText;
}
```

And a topic FooTopic for that data type.

This is how you create a subscriber

```

Participant* participant = Participant::getInstance("FooDomain");
participant->addTypeSupport(new Foo::FooTypeFactory());

Topic topic = participant->createTopic("FooTopic");

FooDataSubscriber sub(topic);

...

```

This is how you create a publisher

```

ops::Participant* participant = Participant::getInstance("FooDomain");
participant->addTypeSupport(new Foo::FooTypeFactory());

Topic topic = participant->createTopic("FooTopic");

FooDataPublisher pub(topic);

...


```