unit uExamplePublisher;

interface

procedure PublisherExample;

implementation

uses
  Windows,
  SysUtils,
  uOps.Error,
  uOps.Participant,
  uOps.Topic,
  TestAll.TestAllTypeFactory,
  TestAll.ChildData;

type
  TStdOutLogger = class(TObject)
  public
    procedure OnErrorReport(Sender : TObject; Error : TError);
  end;

procedure TStdOutLogger.OnErrorReport(Sender : TObject; Error : TError);
begin
  Writeln(Error.getMessage);
end;

procedure PublisherExample;
var
  Logger : TStdOutLogger;
  participant : TParticipant;
  topic : TTopic;
	pub : ChildDataPublisher;
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

	// Create the topic to publish on, might throw ops::NoSuchTopicException
	// The topic must exist in the used ops configuration file
	topic := participant.getTopic('ChildTopic');

	writeln('Publishing on ' + string(topic.Name) +
		' [' + string(topic.Transport) +
		'::' + string(topic.DomainAddress) +
		'::' + IntToStr(topic.Port) +
		'] ');

	// Create a publisher for ChildData
	pub := ChildDataPublisher.Create(topic);

	// It's a good practise to set a publisher name, but it's not necessary.
	pub.Name := 'TestAllPublisher';

	// If there are many publishers on the same topic, a key can be set to identify a
	// specific publisher instance. A subscriber can tell OPS to filter on this key.
	pub.Key := 'InstanceOne';

	// Create some data to publish
	data := ChildData.Create;
	data.baseText := 'Some text';
	data.l := 0;

	while True do begin
		// Change data
		Inc(data.l);

		Writeln('Writing ChildTopic ' + IntToStr(data.l));
		pub.write(data);

		Sleep(1000);
	end;

  // TBD if loop is exited we need to do cleanup
end;

end.
