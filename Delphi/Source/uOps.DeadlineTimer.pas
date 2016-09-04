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
    FTimerHandle: THandle;

    procedure Timeout;

  public
    constructor Create(Owner : TObject);
    destructor Destroy; override;

    // Starts/Re-starts the timer
    procedure Start(timeoutMS : Int32);

    // Cancels a started timer
    procedure Cancel;
  end;

implementation

uses Windows, SysUtils;

// Called by a thread on Windows default thread pool
procedure TimerCallback(Timer: TDeadlineTimer; TimerOrWaitFired: Boolean); stdcall;
begin
  try
    Timer.Timeout;
  except
  end;
end;

{ TDeadlineTimer }

constructor TDeadlineTimer.Create(Owner : TObject);
begin
  inherited Create(Owner);
  FTimerHandle := INVALID_HANDLE_VALUE;
end;

destructor TDeadlineTimer.Destroy;
begin
  Cancel;
  inherited;
end;

procedure TDeadlineTimer.Start(timeoutMS: Int32);
begin
  Cancel;
  if not CreateTimerQueueTimer(FTimerHandle, 0, @TimerCallback, Self, timeoutMS, timeoutMS, 0) then begin
    FTimerHandle := INVALID_HANDLE_VALUE;
  end;
end;

procedure TDeadlineTimer.Cancel;
begin
  if FTimerHandle <> INVALID_HANDLE_VALUE then begin
    DeleteTimerQueueTimer(0, FTimerHandle, INVALID_HANDLE_VALUE);
  end;
  FTimerHandle := INVALID_HANDLE_VALUE;
end;

procedure TDeadlineTimer.Timeout;
begin
  DoNotify(0);
end;

end.

