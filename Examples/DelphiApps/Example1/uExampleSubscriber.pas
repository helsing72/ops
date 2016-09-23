unit uExampleSubscriber;

interface

procedure PollingSubscriberExample;
procedure CallbackSubscriberExample;

implementation

uses
  Windows,
  SysUtils,
  uOps.Error,
  uOps.Participant,
  uOps.Topic,
  uOps.OpsMessage,
  uOps.KeyFilterQoSPolicy,
  TestAll.TestAllTypeFactory,
  TestAll.ChildData;

(******************************************************************************
*
******************************************************************************)
type
  TStdOutLogger = class(TObject)
  public
    procedure OnErrorReport(Sender : TObject; Error : TError);
  end;

procedure TStdOutLogger.OnErrorReport(Sender : TObject; Error : TError);
begin
  Writeln(Error.getMessage);
end;

(******************************************************************************
*
******************************************************************************)
procedure PollingSubscriberExample;
var
  Logger : TStdOutLogger;
  participant : TParticipant;
  topic : TTopic;
	sub : ChildDataSubscriber;
	data : ChildData;
begin
  Logger := TStdOutLogger.Create;

  // Setup the OPS static error service (common for all participants, reports
  // errors during participant creation)
  uOps.Error.gStaticErrorService.addListener(Logger.OnErrorReport);

	// Create OPS participant to access a domain in the default configuration file
	// "ops_config.xml" in current working directory. There are other overloads to
	// create a participant for a specific configuration file.
	participant := TParticipant.getInstance('TestAllDomain');
  if not Assigned(participant) then Exit;

	// Add our generated factory so OPS can create our data objects
	participant.addTypeSupport(TestAllTypeFactory.Create);

	// Setup the participant errorService to catch ev. internal OPS errors
	participant.getErrorService.addListener(logger.OnErrorReport);

	// Create the topic to subscribe to, might throw ops::NoSuchTopicException
	// The topic must exist in the used ops configuration file
	topic := participant.getTopic('ChildTopic');

	writeln('Subscribing on ' + string(topic.Name) +
		' [' + string(topic.Transport) +
		'::' + string(topic.DomainAddress) +
		'::' + IntToStr(topic.Port) +
		'] ');

	// Create a subscriber for ChildData
	sub := ChildDataSubscriber.Create(topic);

	// Setup any filters ...

	// Finally start the subscriber (tell it to start listening for data)
	sub.start();

	while True do begin
{$IFDEF zzz}
    // When using the 'sub.getTypedDataReference()' call we get a reference to an
    // existing message in the OPS core
		if sub.waitForNewData(100) then begin
			// Need to lock message while using it's data via the reference
			sub.aquireMessageLock;
      try
        data := sub.getTypedDataReference;
			  writeln('New data found: Received ChildTopic with ' + IntToStr(data.l));
      finally
        sub.releaseMessageLock;
      end;
		end;
{$ELSE}
    // When using the 'sub.getData()' call we MUST supply an object that
    // will be filled (deep copy) with the latest reveived data
    if not Assigned(data) then data := ChildData.Create;
		if sub.newDataExist then begin
			// Message lock is handled internaly in subscriber while filling in data
			sub.getData(data);
			writeln('New data found: Received ChildTopic with ' + IntToStr(data.l));
		end;
		sleep(10);
{$ENDIF}
	end;

  // TBD if loop is exited we need to do cleanup
end;

(******************************************************************************
*
******************************************************************************)
type
  TSubscriptionHandler = class(TObject)
  private
    FSub : ChildDataSubscriber;
    FLogger : TStdOutLogger;
    FParticipant : TParticipant;
    FTopic : TTopic;
    FData : ChildData;
    procedure OnOpsMessage(Sender : TObject; obj : TOPSMessage);
    procedure OndeadlineMissed(Sender : TObject; int : Integer);
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TSubscriptionHandler.Create;
begin
  FLogger := TStdOutLogger.Create;

  // Setup the OPS static error service (common for all participants, reports
  // errors during participant creation)
  uOps.Error.gStaticErrorService.addListener(FLogger.OnErrorReport);

	// Create OPS participant to access a domain in the default configuration file
	// "ops_config.xml" in current working directory. There are other overloads to
	// create a participant for a specific configuration file.
	FParticipant := TParticipant.getInstance('TestAllDomain');
  if not Assigned(FParticipant) then Exit;

	// Add our generated factory so OPS can create our data objects
	FParticipant.addTypeSupport(TestAllTypeFactory.Create);

	// Setup the participant errorService to catch ev. internal OPS errors
	FParticipant.getErrorService.addListener(FLogger.OnErrorReport);

	// Create the topic to subscribe to, might throw ops::NoSuchTopicException
	// The topic must exist in the used ops configuration file
	FTopic := FParticipant.getTopic('ChildTopic');

	Writeln('Subscribing on ' + string(FTopic.Name) +
		' [' + string(FTopic.Transport) +
		'::' + string(FTopic.DomainAddress) +
		'::' + IntToStr(FTopic.Port) +
		'] ');

	// Create a subscriber for ChildData
	FSub := ChildDataSubscriber.Create(FTopic);

  // Add this class instance as listener for data, the given method will be called
  FSub.AddDataListener(OnOpsMessage);

	// If you want, set a deadline with maximum allowed distance between two messages,
	// the method onDeadlineMissed() will be called if time exceeds set time
	FSub.AddDeadlineListener(OndeadlineMissed);
	FSub.DeadlineQoS := 5000;

  // If you want, add a keyfilter to just be notified with data objects with the specified key
	FSub.AddFilterQoSPolicy(TKeyFilterQoSPolicy.Create('InstanceOne'));

	// There are also some other filters that can be set, and we can implement our own
	//FSub.TimeBasedFilterQoS := 100;

	// Finally start the subscriber (tell it to start listening for data)
	FSub.start;
end;

destructor TSubscriptionHandler.Destroy;
begin
  // TBD if Object should be destroyed we need to do cleanup
  inherited;
end;

procedure TSubscriptionHandler.OnOpsMessage(Sender : TObject; obj : TOPSMessage);
begin
	// NOTE: It's important that we keep this callback fast, it will block
	// further reception for this topic and all other topics that use the same
  // transport (mc/tcp/udp, port, ...)

	// Message lock is automatically held by OPS core while in callback

  // If the class subscribes for more than one topic, they may all come here
  // depending on how you add data listeners
	// So check which it is
	if Sender = FSub then begin
  	// The OPSMessage contains some metadata for the received message
		// eg. publisher name, publication id (message counter), ...
    //   obj.getSource(...);
    //   obj.PublicationID;
    //   obj.PublisherName;
    //   obj.TopicName;
    //   ...

		// Get the actual data object published
		FData := obj.Data as ChildData;

		// Use the data
		Writeln('Received ChildTopic in callback with ' + IntToStr(FData.l));

		// NOTE that the OPSMessage instance and the data object, as default
		// will be deleted when this callback returns.
		// If you eg. want to buffer messages to keep the callback fast, you can
		// postpone the delete by calling "obj.reserve" here in the callback.
		// When you are finished with the buffered message, call "obj.unreserve".
		// This will delete the message if the reserve count == 0 (ie. if the number
		// of reserve() and unreserve() calls match.
	end;
end;

procedure TSubscriptionHandler.OndeadlineMissed(Sender : TObject; int : Integer);
begin
  Writeln('Deadline Missed for topic ' + string(FSub.Topic.Name));
end;

procedure CallbackSubscriberExample;
var
  handler : TSubscriptionHandler;
begin
  handler := TSubscriptionHandler.Create;

	while Assigned(handler) do begin
		sleep(1000);
	end;

  // TBD if loop is exited we need to implement cleanup in TSubscriptionHandler.Destroy()
end;

end.
