
with Text_IO; use Text_IO;

with Ops_Pa,
     Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.Participant_Pa,
     Ops_Pa.PublisherAbs_Pa.Publisher_Pa,
     Ops_Pa.OpsObject_Pa.TestAll_ChildData,
     Ops_Pa.PublisherAbs_Pa.Publisher_Pa.TestAll_ChildData,
     Ops_Pa.SerializableFactory_Pa.TestAll_TestAllTypeFactory;

use  Ops_Pa,
     Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.Participant_Pa,
     Ops_Pa.PublisherAbs_Pa.Publisher_Pa,
     Ops_Pa.OpsObject_Pa.TestAll_ChildData,
     Ops_Pa.PublisherAbs_Pa.Publisher_Pa.TestAll_ChildData,
     Ops_Pa.SerializableFactory_Pa.TestAll_TestAllTypeFactory;

package body ExamplePublisher_Pa is

  use type Int64;

  procedure ErrorLog(Sender : in out Ops_Class_At; Item : Ops_Pa.Error_Pa.Error_Class_At; Arg : Ops_Class_At) is
  begin
    Put_Line("Error:: " & Item.GetMessage);
  end;

  procedure PublisherExample is
    participant : Participant_Class_At;
    topic : Topic_Class_At;
    pub : ChildDataPublisher_Class_At;
    data : ChildData_Class_At;
  begin
    -- Setup the OPS static error service (common for all participants, reports
    -- errors during participant creation)
    StaticErrorService.addListener(ErrorLog'Access, null);

    -- Create OPS participant to access a domain in the default configuration file
    -- "ops_config.xml" in current working directory. There are other overloads to
    -- create a participant for a specific configuration file.
    participant := getInstance("TestAllDomain");
    if participant = null then
      Put_Line("participant == NULL");
      return;
    end if;

    -- Setup the participant errorService to catch ev. internal OPS errors
    participant.getErrorService.addListener(ErrorLog'Access, null);

    -- Create the topic to publish on, might throw ops::NoSuchTopicException
    -- The topic must exist in the used ops configuration file
    topic := participant.getTopic("ChildTopic");

    Put_Line("Publishing on " & topic.Name &
               " [" & topic.Transport &
               "::" & topic.DomainAddress &
               "::" & Int32'Image(topic.Port) &
               "] ");

    -- Create a publisher for ChildData
    pub := Create(topic);

    -- It"s a good practise to set a publisher name, but it"s not necessary.
    pub.SetName( "TestAllPublisher" );

    -- If there are many publishers on the same topic, a key can be set to identify a
    -- specific publisher instance. A subscriber can tell OPS to filter on this key.
    pub.SetKey( "InstanceOne" );

    -- Create some data to publish
    data := Create;
    data.baseText := Copy("Some text");
    data.l := 0;

    while True loop
      -- Change data
      data.l := data.l + 1;

      Put_Line("Writing ChildTopic " & Int64'Image(data.l));
      pub.Write(data);

      delay 1.0;
    end loop;

    -- TBD if loop is exited we need to do cleanup
  end;

end ExamplePublisher_Pa;


