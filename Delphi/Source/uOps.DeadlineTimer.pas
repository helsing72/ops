unit uOps.DeadlineTimer;

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

uses uNotifier;

type
  TDeadlineTimer = class(TNotifier<Integer>)
  private
    // Internal time checked/updated using TInterlocked methods
    FAbsTime : TDateTime;
  public
    constructor Create(Owner : TObject);
    destructor Destroy; override;

    // Starts/Re-starts the timer
    procedure Start(timeoutMS : Int32);

    // Cancels a started timer
    procedure Cancel;
  end;

implementation

uses Windows, Classes, SysUtils,
     System.SyncObjs,
     System.Timespan,
     System.Generics.Defaults,
     System.Generics.Collections,
     System.DateUtils;

const
  cInfinite : TDateTime = 0;

type
  // Single thread that handles all deadline timers
  TDeadlineThread = class(TThread)
  private
    { Private declarations }
    FTimers : TObjectList<TDeadlineTimer>;
    FMutex : TMutex;
    FEvent : TEvent;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Terminate;

    procedure Add(timer : TDeadlineTimer);
    procedure Remove(timer : TDeadlineTimer);
    procedure SignalChange(timer : TDeadlineTimer);
  end;

  // Declare a new custom comparer.
  TDeadlineTimerComparer = class(TComparer<TDeadlineTimer>)
  public
    function Compare(const Left, Right: TDeadlineTimer): Integer; override;
  end;

var
  gThread : TDeadlineThread;

{ TDeadlineThread }

constructor TDeadlineThread.Create;
begin
  inherited;
  // List doesn't own the timers
  FTimers := TObjectList<TDeadlineTimer>.Create(False);
  FMutex := TMutex.Create;
  FEvent := TEvent.Create(nil, False, False, '');
end;

destructor TDeadlineThread.Destroy;
begin
  Terminate;
  WaitFor;
  FreeAndNil(FEvent);
  FreeAndNil(FMutex);
  FreeAndNil(FTimers);
  inherited;
end;

procedure TDeadlineThread.Add(timer: TDeadlineTimer);
begin
  FMutex.Acquire;
  try
    FTimers.Add(timer);
  finally
    FMutex.Release;
  end;
end;

//procedure TDeadlineThread.Add(timer: TDeadlineTimer);
//var
//  OldIdx, NewIdx : Integer;
//begin
//  FMutex.Acquire;
//  try
//    // If in list remove from list
//    OldIdx := FTimers.Remove(timer);
//    // Add timer sorted on timer.AbsTime
//    NewIdx := 0;
//    while NewIdx < FTimers.Count do begin
//      if timer.FAbsTime < FTimers[NewIdx].FAbsTime then Break;
//      Inc(NewIdx);
//    end;
//    FTimers.Insert(NewIdx, timer);
//
//    // If old was placed first or new is placed first, signal thread
//    if (OldIdx = 0) or (NewIdx = 0) then begin
//      FEvent.SetEvent;
//    end;
//
//  finally
//    FMutex.Release;
//  end;
//end;

procedure TDeadlineThread.Remove(timer: TDeadlineTimer);
var
  OldIdx : Integer;
begin
  FMutex.Acquire;
  try
    // If in list remove from list
    OldIdx := FTimers.Remove(timer);
    // If it was placed first, signal thread
    if OldIdx = 0 then begin
      FEvent.SetEvent;
    end;
  finally
    FMutex.Release;
  end;
end;

procedure TDeadlineThread.SignalChange(timer: TDeadlineTimer);
begin
  FEvent.SetEvent;
end;

procedure TDeadlineThread.Terminate;
begin
  inherited Terminate;
  FEvent.SetEvent;
end;

procedure TDeadlineThread.Execute;
var
  NextTime : TDateTime;
  More : Boolean;
  Span : TTimeSpan;
begin
  { Place thread code here }
  while not Terminated do begin
    FMutex.Acquire;
    try
      // Sort list according to FAbsTime taking into account the special cInfinite value
///TODO
/// funkar väl inte då tid kan ändras när som !!!!
      //
      repeat
        // Get time for first timer if any, otherwise infinite
        NextTime := 0;
        if FTimers.Count > 0 then NextTime := FTimers[0].FAbsTime;

        // If first timer has timed out, remove it and doNotify()
        More := False;
        if (NextTime > 0) and (Now >= NextTime) then begin
          try
            FTimers[0].doNotify(0);
          except
          end;
          FTimers.Delete(0);
          More := True;
        end;

        // If removed, re-evaluate timer list
      until not More;
    finally
      FMutex.Release;
    end;

    if NextTime > 0 then begin
      // Wait for event with timeout
      Span := TTimeSpan.Subtract(NextTime, Now);
      if Span.Ticks > 0 then FEvent.WaitFor(Span);
    end else begin
      // Infinite wait
      FEvent.WaitFor;
    end;
  end;
end;

{ TDeadlineTimer }

constructor TDeadlineTimer.Create(Owner : TObject);
begin
  inherited Create(Owner);
  FAbsTime := cInfinite;
  gThread.Add(Self);
end;

destructor TDeadlineTimer.Destroy;
begin
  gThread.Remove(Self);
  inherited;
end;

procedure TDeadlineTimer.Start(timeoutMS: Int32);
var
  newTime, oldTime : TDateTime;
begin
  newTime := Now;
  IncMilliSecond(newTime, timeoutMS);
  oldTime := newTime;
  TInterlocked.Exchange(PInt64(@FAbsTime)^, PInt64(@oldTime)^);
  // Now oldTime realy is the old time and NewTime the new one

  // We only need to signal thread if newTime < oldTime taking into account the special cInfinite value
  if (newTime <> cInfinite) and ( (oldTime = cInfinite) or (newTime < oldTime) )
  then begin
    gThread.SignalChange(Self);
  end;
end;

procedure TDeadlineTimer.Cancel;
var
  newTime : TDateTime;
begin
  newTime := cInfinite;
  TInterlocked.Exchange(PInt64(@FAbsTime)^, PInt64(@newTime)^);
end;

{ TDeadlineTimerComparer }

function TDeadlineTimerComparer.Compare(const Left,
  Right: TDeadlineTimer): Integer;
//var
//  LeftTerm, RightTerm: TDateTime;
begin
  Result := 0;
//  LeftTerm := Left.FAbsTime;
//  RightTerm := Right.FAbsTime;
//  Result := Integer(LeftTerm - RightTerm);
end;

initialization
  System.Assert(SizeOf(TDateTime) = SizeOf(Int64), 'TDateTime has NOT the assumed size');
  gThread := TDeadlineThread.Create;

finalization
  FreeAndNil(gThread);

end.

