# Naming Conventions #

There are a few naming conventions, or guidelines, for data types and topics which are useful and helps keeping concepts clear.

  1. IDL classes' name should end with **Data**
> > E.g. If you want a data type for temprature you should call it TemperatureData rather then Temperature.
> > This prevents you from mixing IDL data types with application code and classes. Keeping with the temperature example it is likely that you would have a class Temperature in your domain model with attached methods, e.g. getAsKelvin(). Thus using TemperatureData clearly separates the data you want to communicate over OPS from your domain classes.
  1. Topic names should end with **Topic**.
> > This clearly states that this is a topic and not a data type, something that is often mixed up when you are new to pub-sub. Moreover, if you have Topics with a 1:1 relationship with a data type you should use the data type prefix (removing the Data in the end) and add Topic as suffix. I.e. if you have a TempratureData and only one Topic for that data type, you should call it TempratureTopic. Note however that it is encouraged to reuse data types for different topics, E.g. you could have an EngineTempratureTopic and an OutdoorTempratureTopic on the same data type TempratureData if you were to model a car control system.
  1. Don't forget packages for topics!
> > A topic name is just a string, but if you build complex system you will be helped by using psuedo namespaces for your topics, especially if you use third party components that also use  OPS. In the temprature example again, a more correct name for your topic would probably be **org.mycarproject.domaintopics.EngineTempratureTopic** or something similar.