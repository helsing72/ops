
with Text_IO; use Text_IO;

with Ops_Pa,
     Ops_Pa.Error_Pa,
     Ops_Pa.Notifier_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
     Ops_Pa.Participant_Pa,
     Ops_Pa.Subscriber_Pa,
     Ops_Pa.OpsObject_Pa.TestAll_ChildData,
     Ops_Pa.Subscriber_Pa.TestAll_ChildData,
     Ops_Pa.FilterQoSPolicy_Pa,
     Ops_Pa.FilterQoSPolicy_Pa.KeyFilterQoSPolicy_Pa,
     Ops_Pa.SerializableFactory_Pa,
     Ops_Pa.SerializableFactory_Pa.TestAll_TestAllTypeFactory;

use  Ops_Pa,
     Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
     Ops_Pa.Participant_Pa,
     Ops_Pa.Subscriber_Pa,
     Ops_Pa.OpsObject_Pa.TestAll_ChildData,
     Ops_Pa.Subscriber_Pa.TestAll_ChildData,
     Ops_Pa.FilterQoSPolicy_Pa,
     Ops_Pa.FilterQoSPolicy_Pa.KeyFilterQoSPolicy_Pa,
     Ops_Pa.SerializableFactory_Pa,
     Ops_Pa.SerializableFactory_Pa.TestAll_TestAllTypeFactory;

package body ExampleSubscriber_Pa is

  procedure ErrorLog(Sender : in out Ops_Class_At; Item : Ops_Pa.Error_Pa.Error_Class_At; Arg : Ops_Class_At) is
  begin
    Put_Line("Error:: " & Item.GetMessage);
  end;

  procedure PollingSubscriberExample is
    participant : Participant_Class_At;
    topic : Topic_Class_At;
    sub : ChildDataSubscriber_Class_At;
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

    -- Add our generated factory so OPS can create our data objects
    declare
      fact : TestAllTypeFactory_Class_At := Create;
    begin
      participant.addTypeSupport( SerializableFactory_Class_At(fact) );
    end;

    -- Create the topic to publish on, might throw ops::NoSuchTopicException
    -- The topic must exist in the used ops configuration file
    topic := participant.getTopic("ChildTopic");

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
      if True then
        -- When using the 'sub.getTypedDataReference()' call we get a reference to an
        -- existing message in the OPS core
        if sub.waitForNewData(100) then
          -- Need to lock message while using it's data via the reference
          sub.acquireMessageLock;
          begin
            data := sub.getTypedDataReference;
            Put_Line("New data found: Received ChildTopic with " & Int64'Image(data.l));
            sub.releaseMessageLock;
          exception
            when others =>
              sub.releaseMessageLock;
          end;
        end if;
      else
        -- When using the 'sub.getData()' call we MUST supply an object that
        -- will be filled (deep copy) with the latest reveived data
        if data = null then
          data := Create;
        end if;
        if sub.newDataExist then
          -- Message lock is handled internaly in subscriber while filling in data
          if sub.getData(data) then
            Put_Line("New data found: Received ChildTopic with " & Int64'Image(data.l));
          else
            Put_Line("Failed to get data !!!");
          end if;
        end if;
        delay 0.01;
      end if;
    end loop;

    -- TBD if loop is exited we need to do cleanup
  end PollingSubscriberExample;

  -- ======================================================================

  package Z is

    type SubscriptionHandler_Class is
      new Ops_Class and
      Ops_Pa.Subscriber_Pa.MessageNotifier_Pa.Listener_Interface and
      OPs_Pa.Subscriber_Pa.DeadlineNotifier_Pa.Listener_Interface with
        private;
    type SubscriptionHandler_Class_At is access all SubscriptionHandler_Class'Class;

    procedure OnNotify( Self : in out SubscriptionHandler_Class; Sender : in Ops_Class_At; Item : in OPSMessage_Class_At );
    procedure OnNotify( Self : in out SubscriptionHandler_Class; Sender : in Ops_Class_At; Item : in Integer );

    function Create return SubscriptionHandler_Class_At;

  private
    type SubscriptionHandler_Class is
      new Ops_Class and
      Ops_Pa.Subscriber_Pa.MessageNotifier_Pa.Listener_Interface and
      OPs_Pa.Subscriber_Pa.DeadlineNotifier_Pa.Listener_Interface with
      record
        SelfAt : SubscriptionHandler_Class_At;
        Sub : ChildDataSubscriber_Class_At;
        Participant : Participant_Class_At;
        Topic : Topic_Class_At;
        Data : ChildData_Class_At;
      end record;

    procedure InitInstance( Self : in out SubscriptionHandler_Class; SelfAt : SubscriptionHandler_Class_At );
    procedure Finalize( Self : in out SubscriptionHandler_Class );
  end Z;

  package body Z is
    function Create return SubscriptionHandler_Class_At is
      Self : SubscriptionHandler_Class_At := null;
    begin
      Self := new SubscriptionHandler_Class;
      InitInstance( Self.all, Self );
      return Self;
    exception
      when others =>
        Free(Self);
        raise;
    end;

    procedure InitInstance( Self : in out SubscriptionHandler_Class; SelfAt : SubscriptionHandler_Class_At ) is
    begin
      Self.SelfAt := SelfAt;

      -- Setup the OPS static error service (common for all participants, reports
      -- errors during participant creation)
      StaticErrorService.addListener(ErrorLog'Access, null);

      -- Create OPS participant to access a domain in the default configuration file
      -- "ops_config.xml" in current working directory. There are other overloads to
      -- create a participant for a specific configuration file.
      Self.Participant := getInstance("TestAllDomain");
      if Self.Participant = null then
        Put_Line("participant == NULL");
        return;
      end if;

      -- Setup the participant errorService to catch ev. internal OPS errors
      Self.Participant.getErrorService.addListener(ErrorLog'Access, null);

      -- Add our generated factory so OPS can create our data objects
      declare
        fact : TestAllTypeFactory_Class_At := Create;
      begin
        Self.participant.addTypeSupport( SerializableFactory_Class_At(fact) );
      end;

      -- Create the topic to subscribe to, might throw ops::NoSuchTopicException
      -- The topic must exist in the used ops configuration file
      Self.topic := Self.Participant.getTopic("ChildTopic");

      Put_Line("Subscribing on " & Self.topic.Name &
                 " [" & Self.topic.Transport &
                 "::" & Self.topic.DomainAddress &
                 "::" & Int32'Image(Self.topic.Port) &
                 "] ");

      -- Create a subscriber for ChildData
      Self.Sub := Create(Self.Topic);

      -- Add this class instance as listener for data, the given method will be called
      Self.Sub.AddListener( Ops_Pa.Subscriber_Pa.MessageNotifier_Pa.Listener_Interface_At(Self.SelfAt) );

      -- If you want, set a deadline with maximum allowed distance between two messages,
      -- the method onDeadlineMissed() will be called if time exceeds set time
--      Self.Sub.AddDeadlineListener(OndeadlineMissed);
--      Self.Sub.DeadlineQoS := 5000;

      -- If you want, add a keyfilter to just be notified with data objects with the specified key
      declare
        filt : KeyFilterQoSPolicy_Class_At := Create( key => "InstanceOne" );
      begin
        Self.Sub.AddFilterQoSPolicy( FilterQoSPolicy_Class_At(filt) );
      end;

      -- There are also some other filters that can be set, and we can implement our own
      --Self.Sub.SetTimeBasedFilterQoS( 100 );

      -- Finally start the subscriber (tell it to start listening for data)
      Self.Sub.start;
    end;

    procedure Finalize( Self : in out SubscriptionHandler_Class ) is
    begin
      -- TBD if Object should be destroyed we need to do cleanup
      null;
    end;

    procedure OnNotify( Self : in out SubscriptionHandler_Class; Sender : in Ops_Class_At; Item : in OPSMessage_Class_At ) is
    begin
      -- NOTE: It's important that we keep this callback fast, it will block
      -- further reception for this topic and all other topics that use the same
      -- transport (mc/tcp/udp, port, ...)

      -- Message lock is automatically held by OPS core while in callback

      -- If the class subscribes for more than one topic, they may all come here
      -- depending on how you add data listeners
      -- So check which it is
      if Sender = Ops_Class_At(Self.Sub) then
        -- The OPSMessage contains some metadata for the received message
        -- eg. publisher name, publication id (message counter), ...
        --   obj.getSource(...);
        --   obj.PublicationID;
        --   obj.PublisherName;
        --   obj.TopicName;
        --   ...

        -- Get the actual data object published
        Self.Data := ChildData_Class_At(Item.Data);

        -- Use the data
        Put_Line("Received ChildTopic in callback with " & Int64'Image(Self.Data.l));

        -- NOTE that the OPSMessage instance and the data object, as default
        -- will be deleted when this callback returns.
        -- If you eg. want to buffer messages to keep the callback fast, you can
        -- postpone the delete by calling "obj.reserve" here in the callback.
        -- When you are finished with the buffered message, call "obj.unreserve".
        -- This will delete the message if the reserve count == 0 (ie. if the number
        -- of reserve() and unreserve() calls match.
      end if;
    end;

    procedure OnNotify( Self : in out SubscriptionHandler_Class; Sender : in Ops_Class_At; Item : in Integer ) is
    begin
      Put_line("Deadline Missed for topic " & Self.Sub.Topic.Name);
    end;

  end Z;

  use Z;

  procedure CallbackSubscriberExample is
    handler : SubscriptionHandler_Class_At;
  begin
    handler := Create;

    while handler /= null loop
      delay 1.0;
    end loop;

    -- TBD if loop is exited we need to implement cleanup in TSubscriptionHandler.Destroy()
  end;

end ExampleSubscriber_Pa;

