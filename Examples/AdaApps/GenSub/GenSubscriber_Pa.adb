
with Text_IO; use Text_IO;

with Ops_Pa,
     Ops_Pa.Error_Pa,
     Ops_Pa.Notifier_Pa,
     Ops_Pa.OpsObject_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
     Ops_Pa.Participant_Pa,
     Ops_Pa.Subscriber_Pa,
     Ops_Pa.FilterQoSPolicy_Pa,
     Ops_Pa.FilterQoSPolicy_Pa.KeyFilterQoSPolicy_Pa,
     Ops_Pa.SerializableFactory_Pa,
     Ops_Pa.SerializableFactory_Pa.GenericTypeFactory;

use  Ops_Pa,
     Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
     Ops_Pa.Participant_Pa,
     Ops_Pa.Subscriber_Pa,
     Ops_Pa.FilterQoSPolicy_Pa,
     Ops_Pa.FilterQoSPolicy_Pa.KeyFilterQoSPolicy_Pa,
     Ops_Pa.SerializableFactory_Pa,
     Ops_Pa.SerializableFactory_Pa.GenericTypeFactory;

package body GenSubscriber_Pa is

  procedure ErrorLog(Sender : in out Ops_Class_At; Item : Ops_Pa.Error_Pa.Error_Class_At; Arg : Ops_Class_At) is
  begin
    Put_Line("Error:: " & Item.GetMessage);
  end;

  procedure Listener(cfgfile : String; domainName : String; topicName : String) is
    participant : Participant_Class_At;
    topic : Topic_Class_At;
    sub : Subscriber_Class_At;
    mess : OPSMessage_Class_At;
--    data : OpsObject_Class_At;
  begin
    -- Setup the OPS static error service (common for all participants, reports
    -- errors during participant creation)
    StaticErrorService.addListener(ErrorLog'Access, null);

    -- Create OPS participant to access a domain in the default configuration file
    -- "ops_config.xml" in current working directory. There are other overloads to
    -- create a participant for a specific configuration file.
    participant := getInstance(domainName, domainName, cfgfile);
    if participant = null then
      Put_Line("participant == NULL");
      return;
    end if;

    -- Setup the participant errorService to catch ev. internal OPS errors
    participant.getErrorService.addListener(ErrorLog'Access, null);

    -- Add our generated factory so OPS can create our data objects
    declare
      fact : GenericTypeFactory_Class_At := Create;
    begin
      participant.addTypeSupport( SerializableFactory_Class_At(fact) );
    end;

    -- Create the topic to publish on, might throw ops::NoSuchTopicException
    -- The topic must exist in the used ops configuration file
    topic := participant.getTopic(topicName);

    Put_Line("Subscribing on " & topic.Name &
               " [" & topic.Transport &
               "::" & topic.DomainAddress &
               "::" & Int32'Image(topic.Port) &
               "] ");


    -- Create a subscriber for ChildData
    sub := Create(topic);

    -- Setup any filters ...

    -- Finally start the subscriber (tell it to start listening for data)
    sub.start;

    while True loop
      if sub.waitForNewData(100) then
        -- Need to lock message while using it's data via the reference
        sub.acquireMessageLock;
        begin
          mess := sub.getMessage;
          Put_Line("New data found");
          sub.releaseMessageLock;
        exception
          when others =>
            sub.releaseMessageLock;
        end;
      end if;
    end loop;

    -- TBD if loop is exited we need to do cleanup
  end Listener;

end GenSubscriber_Pa;
