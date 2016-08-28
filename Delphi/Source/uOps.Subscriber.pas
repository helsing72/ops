unit uOps.Subscriber;

(**
*
* Copyright (C) 2016 Lennart Andersson.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*)

interface

uses System.Generics.Collections,
     System.SyncObjs,
     uNotifier,
     uOps.Types,
     uOps.Topic,
     uOps.ByteBuffer,
     uOps.MemoryMap,
     uOps.OPSArchiverIn,
     uOps.FilterQoSPolicy,
     uOps.DeadlineTimer,
     uOps.Transport.ReceiveDataHandler,
     uOps.Participant,
     uOps.OpsObject,
     uOps.OpsMessage;

type
  TSubscriber = class(TObject)
  private
    // Name of this subscriber
    FName : string;

    // The Topic this Subscriber subscribes to (NOTE: we don't own the object)
    FTopic : TTopic;

    // Subscriber started or not
    FStarted : Boolean;

    // Used for notifications to users of the subscriber
    FDataNotifier : TNotifier<TOPSMessage>;

    // Used for notifications to users of the subscriber
    FDeadlineNotifier : TDeadlineTimer;

    // The Participant to which this Subscriber belongs (NOTE: we don't own the object)
    FParticipant : TParticipant;

    // ReceiveDataHandler delivering new data samples to this Subscriber (NOTE: we don't own the object)
    FReceiveDataHandler : TReceiveDataHandler;

    // Receiver side filters that will be applied to data from receiveDataHandler
    // before delivery to application layer.
    FFilterQoSPolicies : TObjectList<TFilterQoSPolicy>;
    FFilterQoSPolicyMutex : TMutex;

//    std::deque<OPSMessage*> messageBuffer;
//    unsigned int messageBufferMaxSize;
//    void addToBuffer(OPSMessage* mess);

    // Used for wakeup of thread waiting for new data in 'WaitForNewData()'
    FNewDataEvent : TEvent;

    // Time used for deadline missed checks
    FTimeLastData : Int64;
    FDeadlineTimeout : Int64;
    FDeadlineMissed : Boolean;

    // Time used for time based filter
    FTimeLastDataForTimeBase : Int64;
    FTimeBaseMinSeparationTime : Int64;

    function ApplyFilterQoSPolicies(o : TOPSObject) : Boolean;

//    void registerForDeadlineTimeouts();
//    void cancelDeadlineTimeouts();

    procedure SetDeadlineQoS(millis : Int64);
    function GetNumReservedMessages : Integer;

  protected
    FMessage : TOPSMessage;
    FData : TOPSObject;

    FFirstDataReceived : Boolean;
    FHasUnreadData : Boolean;

    function getData : TOPSObject;
    procedure checkAndNotifyDeadlineMissed;

    // Message listener callback
    procedure onNewMessage(Sender : TObject; arg : TOPSMessage);

    // Deadline listener callback
    procedure onNewEvent(Sender : TObject; arg : Integer);

  public
    constructor Create(t : TTopic);
    destructor Destroy; override;

    // Starts communication.
    procedure Start;

    // Stops communication, unsubscribe this subscriber from data.
    procedure Stop;

    // Add notifications (callbacks) when data arrives
    // NOTE: 'Proc' will be called in the context of an arbitrary thread
    procedure AddDataListener(Proc : TOnNotifyEvent<TOPSMessage>);

    // Add notifications (callbacks) when a deadline is missed
    // NOTE: 'Proc' will be called in the context of an arbitrary thread
    procedure AddDeadlineListener(Proc : TOnNotifyEvent<Integer>);

    // Add the given object and take ownership over it.
    procedure AddFilterQoSPolicy(fqos : TFilterQoSPolicy);

    // Remove the given object from the list and ownership are returned to the caller.
    procedure RemoveFilterQoSPolicy(fqos : TFilterQoSPolicy);

    // Waits for new data to arrive or timeout.
    // Returns: true if new data (i.e. unread data) exist.
    function WaitForNewData(timeoutMS : Integer) : Boolean;

		// Methods for handling the message lock.
    // NOTE: Lock should be held while working with the message using a reference got either by
    //   protected member FData
    //   protected method getData()
    //   public method getMessage()
    //   public method getDataReference()
    function aquireMessageLock : Boolean;
    procedure releaseMessageLock;

    ///Returns a reference to the latest received OPSMessage (including the latest data object).
    ///Clears the "new data" flag.
    ///NOTE: MessageLock should be hold while working with the message, to prevent a
    ///new incomming message to delete the current one while in use.
    function getMessage : TOPSMessage;

    function isDeadlineMissed : Boolean;

//    void setHistoryMaxSize(int s);
//    std::deque<OPSMessage*> getHistory();

    ///Returns a reference the latest received data object.
    ///Clears the "new data" flag.
    ///NOTE: MessageLock should be hold while working with the data object, to prevent a
    ///new incomming message to delete the current one while in use.
    function getDataReference : TOPSObject; virtual;

    property Name : string read FName write FName;
    property Topic : TTopic read FTopic;

    ///Checks if new data exist (same as 'waitForNewData(0)' but faster)
    ///Returns: true if new data (i.e. unread data) exist.
    property NewDataExist : Boolean read FHasUnreadData;

    // The deadline timout [ms] for this subscriber.
    // If no message is received within deadline, listeners to deadlines will be notified
    // == 0, --> Infinite wait
    property DeadlineQoS : Int64 read FDeadlineTimeout write SetDeadlineQoS;

    // Sets the minimum time separation [ms] between to consecutive messages.
    // Received messages in between will be ignored by this Subscriber
    // == 0, no filtering on time
    property TimeBasedFilterQoS : Int64 read FTimeBaseMinSeparationTime write FTimeBaseMinSeparationTime;

    // Returns the number of reserved messages in the underlying ReceiveDataHandler
    // This value is the total nr for this topic on this participant not only
    // for this subscriber.
    property numReservedMessages : Integer read GetNumReservedMessages;
  end;

implementation

uses SysUtils,
     uOps.Error,
     uOps.TimeHelper;

constructor TSubscriber.Create(t : TTopic);
begin
  inherited Create;
  FTopic := t;
  FDataNotifier := TNotifier<TOPSMessage>.Create(Self);
  FDeadlineNotifier := TDeadlineTimer.Create(Self);
  FFilterQoSPolicies := TObjectList<TFilterQoSPolicy>.Create;
  FFilterQoSPolicyMutex := TMutex.Create;
  FNewDataEvent := TEvent.Create(nil, True, False, '');

//    messageBufferMaxSize(1),
  FDeadlineTimeout := 0;

  FParticipant := TParticipant.getInstance(topic.DomainID, topic.ParticipantID);

  FTimeLastData := TTimeHelper.CurrentTimeMillis;
end;

destructor TSubscriber.Destroy;
begin
  FDeadlineNotifier.Cancel;
  if FStarted then Stop;

//		while (messageBuffer.size() > 0) {
//			messageBuffer.back()->unreserve();
//			messageBuffer.pop_back();
//		}

  FreeAndNil(FNewDataEvent);
  FreeAndNil(FFilterQoSPolicyMutex);
  FreeAndNil(FFilterQoSPolicies);
  FreeAndNil(FDeadlineNotifier);
  FreeAndNil(FDataNotifier);
  inherited;
end;

// ---------------------------------------------------------------------------

procedure TSubscriber.Start;
begin
	if FStarted then Exit;

  FReceiveDataHandler := FParticipant.GetReceiveDataHandler(FTopic);
  FReceiveDataHandler.addListener(onNewMessage);

  if FDeadlineTimeout > 0 then FDeadlineNotifier.Start(FDeadlineTimeout);
  FStarted := True;
end;

procedure TSubscriber.Stop;
begin
	if not FStarted then Exit;

  if FDeadlineTimeout > 0 then FDeadlineNotifier.Cancel;

  FReceiveDataHandler.aquireMessageLock;
  FReceiveDataHandler.removeListener(onNewMessage);
  FReceiveDataHandler.releaseMessageLock;
  FReceiveDataHandler := nil;
  FParticipant.ReleaseReceiveDataHandler(FTopic);

  FStarted := False;
end;

// ---------------------------------------------------------------------------

function TSubscriber.GetNumReservedMessages : Integer;
begin
  Result := 0;
//  return FReceiveDataHandler.numReservedMessages();
end;

function TSubscriber.getMessage : TOPSMessage;
begin
  FHasUnreadData := False;
  Result := FMessage;
end;

function TSubscriber.getDataReference : TOPSObject;
begin
  FHasUnreadData := False;
  Result := FData;
end;

function TSubscriber.getData : TOPSObject;
begin
  FHasUnreadData := False;
  Result := FData;
end;

// ---------------------------------------------------------------------------
// Methods need protection by e.g. a Mutex, since Apply....() will be called by the receiving thread

procedure TSubscriber.AddFilterQoSPolicy(fqos : TFilterQoSPolicy);
begin
  if not Assigned(fqos) then Exit;

  FFilterQoSPolicyMutex.Acquire;
  try
    FFilterQoSPolicies.Add(fqos);
  finally
    FFilterQoSPolicyMutex.Release;
  end;
end;

procedure TSubscriber.RemoveFilterQoSPolicy(fqos : TFilterQoSPolicy);
var
  Idx : Integer;
begin
  if not Assigned(fqos) then Exit;

  FFilterQoSPolicyMutex.Acquire;
  try
    Idx := FFilterQoSPolicies.IndexOf(fqos);
    if Idx >= 0 then begin
      // Remove from list without freeing object
      FFilterQoSPolicies.Extract(fqos);
    end;
  finally
    FFilterQoSPolicyMutex.Release;
  end;
end;

function TSubscriber.ApplyFilterQoSPolicies(o : TOPSObject) : Boolean;
var
  fqos : TFilterQoSPolicy;
begin
  Result := True;
  FFilterQoSPolicyMutex.Acquire;
  try
    for fqos in FFilterQoSPolicies do begin
      if not fqos.ApplyFilter(o) then begin
        Result := False;
        Break;
      end;
    end;
  finally
    FFilterQoSPolicyMutex.Release;
  end;
end;

// ---------------------------------------------------------------------------

procedure TSubscriber.AddDataListener(Proc : TOnNotifyEvent<TOPSMessage>);
begin
  FDataNotifier.addListener(Proc);
end;

function TSubscriber.aquireMessageLock : Boolean;
begin
 Result := FReceiveDataHandler.aquireMessageLock;
end;

procedure TSubscriber.releaseMessageLock;
begin
  FReceiveDataHandler.releaseMessageLock;
end;

// Message listener callback (called from receive thread)
// Called with receiveDataHandler MessageLock taken
procedure TSubscriber.onNewMessage(Sender : TObject; arg : TOPSMessage);
var
  o : TOPSObject;
begin
  //Perform a number of checks on incomming data to be sure we want to deliver it to the application layer

  //Check that this message is delivered on the same topic as this Subscriber use
  if arg.TopicName <> FTopic.Name then begin
    // This is a normal case when several Topics use the same port
    Exit;
  end
  //Check that the type of the delivered data can be interpreted as the type we expect in this Subscriber
  else if Pos(FTopic.TypeID, arg.Data.TypesString) <= 0 then begin
    FParticipant.ReportError(TBasicError.Create('Subscriber', 'onNewEvent', 'Received message with wrong data type for Topic'));
    Exit;
  end;

  //OK, we passed the basic checks, lets go on and filter on data content...

  o := arg.Data;
  if applyFilterQoSPolicies(o) then begin
    // Apply timebased filter (if any)
    if ((TTimeHelper.CurrentTimeMillis - FTimeLastDataForTimeBase) > FTimeBaseMinSeparationTime) or
       (FTimeBaseMinSeparationTime = 0)
    then begin
      FFirstDataReceived := true;

//      addToBuffer(arg);
      FMessage := arg;
      FData := o;

      FDataNotifier.doNotify(arg);

      FHasUnreadData := True;
      FNewDataEvent.SetEvent;

      FTimeLastDataForTimeBase := TTimeHelper.CurrentTimeMillis;
      FTimeLastData := TTimeHelper.CurrentTimeMillis;

      if FDeadlineTimeout > 0 then FDeadlineNotifier.Start(FDeadlineTimeout);
      FDeadlineMissed := False;
    end;
  end;
end;

// Waits for new data to arrive or timeout.
// Returns: true if new data (i.e. unread data) exist.
function TSubscriber.WaitForNewData(timeoutMS : Integer) : Boolean;
var
  res : TWaitResult;
begin
  Result := True;
  FNewDataEvent.ResetEvent;

  if FHasUnreadData then Exit;

  res := FNewDataEvent.WaitFor(timeoutMS);
  if res <> wrSignaled then Result := False;
end;

// ---------------------------------------------------------------------------

procedure TSubscriber.AddDeadlineListener(Proc : TOnNotifyEvent<Integer>);
begin
  FDeadlineNotifier.addListener(Proc);
end;

procedure TSubscriber.SetDeadlineQoS(millis : Int64);
begin
	if millis <= 0 then begin
    FDeadlineTimeout := 0;
    FDeadlineNotifier.Cancel;
	end else begin
    FDeadlineTimeout := millis;
    FDeadlineNotifier.Start(FDeadlineTimeout);
  end;
end;

function TSubscriber.isDeadlineMissed : Boolean;
begin
  Result := False;
  if FDeadlineTimeout = 0 then Exit;
  if (TTimeHelper.currentTimeMillis - FTimeLastData) > FDeadlineTimeout then begin
    FDeadlineMissed := True;
    Result := True;
  end;
end;

procedure TSubscriber.checkAndNotifyDeadlineMissed;
begin
// if (isDeadlineMissed())
// {
//   //printf("DeadlineMissed timeLastData = %d, currTime = %d, deadlineTimeout = %d\n", FTimeLastData, currTime, FDeadlineTimeout);
//   deadlineMissedEvent.notifyDeadlineMissed();
//   FTimeLastData := TTimeHelper.CurrentTimeMillis;
// }
end;

// Deadline listener callback
procedure TSubscriber.onNewEvent(Sender : TObject; arg : Integer);
begin
//        deadlineMissedEvent.notifyDeadlineMissed();
//        deadlineTimer->start(deadlineTimeout);
end;

end.

