unit uOps.Transport.SendDataHandler;

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

uses Contnrs,
     System.SyncObjs,
     uOps.Types,
     uOps.Topic,
     uOps.Transport.Sender;

type
  TSendDataHandler = class(TObject)
  private
    FUsers : TObjectList;

  protected
    FSender : TSender;
    FMutex : TMutex;

  public
    constructor Create;
    destructor Destroy; override;

		function sendData(buf : PByte; bufSize : Integer; topic : TTopic) : Boolean; virtual; abstract;

		procedure addUser(client : TObject); virtual;
		procedure removeUser(client : TObject); virtual;
  end;

implementation

uses SysUtils;

constructor TSendDataHandler.Create;
begin
  inherited Create;
  FUsers := TObjectList.Create(False);    // We don't own objects in list
  FMutex := TMutex.Create;
end;

destructor TSendDataHandler.Destroy;
begin
  FreeAndNIl(FMutex);
  FreeAndNil(FUsers);
  inherited;
end;

procedure TSendDataHandler.addUser(client : TObject);
begin
  FMutex.Acquire;
  try
    // Check that it isn't already in the list
    if FUsers.IndexOf(client) >= 0 then Exit;

    // Save client in the list
    FUsers.Add(client);

    // For the first client, we open the sender
    if FUsers.Count = 1 then FSender.Open;
  finally
    FMutex.Release;
  end;
end;

procedure TSendDataHandler.removeUser(client : TObject);
begin
  FMutex.Acquire;
  try
    // Remove it from the list
    FUsers.Extract(client);

    // For the last client, we close the sender
    if FUsers.Count = 0 then FSender.Close;
  finally
    FMutex.Release;
  end;
end;

end.

