unit uOps.PublisherAbs;

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

uses uOps.Types,
     uOps.Topic,
     uOps.OpsObject;

type
  TPublisherAbs = class(TObject)
  protected
    FName : AnsiString;
    FKey : AnsiString;

    // Send behavior parameters
    FSendSleepTime : Int64;
    FSleepEverySendPacket : Integer;
    FSleepOnSendFailed : Boolean;

    // The Topic this Publisher publish on (NOTE: we don't own the object)
    FTopic : TTopic;

  public
    procedure Start; virtual; abstract;
    procedure Stop; virtual; abstract;

    procedure WriteOPSObject(obj : TOPSObject); virtual; abstract;

    property Name : AnsiString read FName write FName;
    property Key : AnsiString read FKey write FKey;
    property Topic : TTopic read FTopic;

    // Send behavior parameters
    property SendSleepTime : Int64 read FSendSleepTime write FSendSleepTime;
    property SleepEverySendPacket : Integer read FSleepEverySendPacket write FSleepEverySendPacket;
    property SleepOnSendFailed : Boolean read FSleepOnSendFailed write FSleepOnSendFailed;
  end;

implementation

end.

