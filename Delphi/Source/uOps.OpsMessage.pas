unit uOps.OpsMessage;

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
     uOps.ArchiverInOut,
     uOps.OpsObject;

//TODO  #include "Reservable.h"

type
  TOPSMessage = class(TOPSObject)  //TODO	, public Reservable
  private
    FMessageType : Byte;          // Serialized (not used, always 0)
    //FEndianness : Byte;         //            (not used)
    FPublisherPriority : Byte;    // Serialized (not used, always 0)
    FDataOwner : Boolean;
    FSourcePort : Integer;
		FSourceIP : string;
    //FQosMask : Int64;
    FPublicationID : Int64;       // Serialized
    FPublisherName : AnsiString;  // Serialized
    FTopicName : AnsiString;      // Serialized
    FTopLevelKey : AnsiString;    // Serialized (not used, empty string)
    FAddress : AnsiString;        // Serialized (not used, empty string)
    FData : TOPSObject;           // Serialized

  public
    constructor Create;
    destructor Destroy; override;

    procedure setSource(addr : string; port : Integer);
    procedure getSource(var addr : string; var port : Integer);

    procedure Serialize(archiver : TArchiverInOut); override;

    property DataOwner : Boolean read FDataOwner write FDataOwner;
    property PublicationID : Int64 read FPublicationID write FPublicationID;
    property PublisherName : AnsiString read FPublisherName write FPublisherName;
    property TopicName : AnsiString read FTopicName write FTopicName;
    property Data : TOPSObject read FData write FData;
  end;

implementation

uses SysUtils;

constructor TOPSMessage.Create;
begin
  inherited;
  FDataOwner := True;
  AppendType('ops.protocol.OPSMessage');
end;

destructor TOPSMessage.Destroy;
begin
  if FDataOwner then begin
    FreeAndNil(FData);
  end;
  inherited;
end;

procedure TOPSMessage.setSource(addr : string; port : Integer);
begin
  FSourceIP := addr;
  FSourcePort := port;
end;

procedure TOPSMessage.getSource(var addr : string; var port : Integer);
begin
  addr := FSourceIP;
  port := FSourcePort;
end;

procedure TOPSMessage.Serialize(archiver : TArchiverInOut);
begin
  inherited Serialize(archiver);

  // Can't change/addto these without breaking compatbility
  archiver.inout('messageType', FMessageType);
  archiver.inout('publisherPriority', FPublisherPriority);
  archiver.inout('publicationID', FPublicationID);
  archiver.inout('publisherName', FPublisherName);
  archiver.inout('topicName', FTopicName);
  archiver.inout('topLevelKey', FTopLevelKey);
  archiver.inout('address', FAddress);
  FData := TOPSObject(archiver.inout2('data', TSerializable(FData)));
end;

end.

