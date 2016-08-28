unit uOps.KeyFilterQoSPolicy;

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

uses Classes,
     System.SyncObjs,
     uOps.OPSObject,
     uOps.FilterQoSPolicy;

type
  TKeyFilterQoSPolicy = class(TFilterQoSPolicy)
  private
    FKeyStrings : TStringList;
    FLock : TCriticalSection;

  public
    // Creates an empty filter that lets all objects thru
    constructor Create; overload;

    // Creates a filter with one key, that must match for objects to come thru
    constructor Create(keyString : string); overload;

    // Creates a filter with N keys, if any matches the object come thru
    // If the provided list is empty, all objects come thru
    constructor Create(keyStrings : TStringList); overload;

    destructor Destroy; override;

    // Replaces current key[s] with the new keys provided
    // If the provided list is empty, all objects come thru
    procedure setKeys(keyStrings : TStringList);

    // Replaces current key[s] with the new single key provided
    procedure setKey(key : string);

    // Returns the current keys in provided list
    procedure getKeys(keyStrings : TStringList);

    // Overides applyFilter(OPSObject* o) in FilterQoSPolicy
    function ApplyFilter(o : TOPSObject) : Boolean; override;

  end;

implementation

uses SysUtils;

// Creates an empty filter that lets all objects thru
constructor TKeyFilterQoSPolicy.Create;
begin
  inherited;
  FLock := TCriticalSection.Create;
  FKeyStrings := TStringList.Create;
end;

// Creates a filter with one key, that must match for objects to come thru
constructor TKeyFilterQoSPolicy.Create(keyString : string);
begin
  Create;
  FKeyStrings.Add(keyString);
end;

// Creates a filter with N keys, if any matches the object come thru
// If the provided vector is empty, all objects come thru
constructor TKeyFilterQoSPolicy.Create(keyStrings : TStringList);
begin
  Create;
  FKeyStrings.AddStrings(keyStrings);
end;

destructor TKeyFilterQoSPolicy.Destroy;
begin
  FreeAndNil(FKeyStrings);
  FreeAndNil(FLock);
  inherited;
end;

// Replaces current key[s] with the new keys provided
// If the provided vector is empty, all objects come thru
procedure TKeyFilterQoSPolicy.setKeys(keyStrings : TStringList);
begin
  FLock.Acquire;
  try
    FKeyStrings.Clear;
    FKeyStrings.AddStrings(keyStrings);
  finally
    FLock.Release;
  end;
end;

// Replaces current key[s] with the new single key provided
procedure TKeyFilterQoSPolicy.setKey(key : string);
begin
  FLock.Acquire;
  try
    FKeyStrings.Clear;
    FKeyStrings.Add(key);
  finally
    FLock.Release;
  end;
end;

// Returns the current keys in the provided list
procedure TKeyFilterQoSPolicy.getKeys(keyStrings : TStringList);
begin
  FLock.Acquire;
  try
    keyStrings.Clear;
    keyStrings.AddStrings(FKeyStrings);
  finally
    FLock.Release;
  end;
end;

// Returning false from a filter indicates that this data sample (OPSObject)
// shall not be propagated to the application layer.
function TKeyFilterQoSPolicy.ApplyFilter(o : TOPSObject) : Boolean;
var
  i : Integer;
  str : string;
begin
  Result := True;
  FLock.Acquire;
  try
    // An empty key filter is the same as no filter, i.e. keep object
    if FKeyStrings.Count = 0 then Exit;

    str := string(o.Key);
    for i := 0 to FKeyStrings.Count - 1 do begin
      if str = FKeyStrings[i] then Exit;  // match, so keep object
    end;

    Result := False;  // no match, skip this object
  finally
    FLock.Release;
  end;
end;

end.

