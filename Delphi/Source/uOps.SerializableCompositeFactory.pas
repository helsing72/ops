unit uOps.SerializableCompositeFactory;

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
     uOps.ArchiverInOut,
     uOps.SerializableFactory;

type
  TSerializableCompositeFactory = class(TSerializableFactory)
  private
    FChildFactories : TObjectList<TSerializableFactory>;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(o : TSerializableFactory);
    function Remove(o : TSerializableFactory) : Boolean;

    function Make(types : string) : TSerializable; override;
  end;

implementation

uses SysUtils;

constructor TSerializableCompositeFactory.Create;
begin
  inherited Create;
  // Objects in list are owned by list
  FChildFactories := TObjectList<TSerializableFactory>.Create;
end;

destructor TSerializableCompositeFactory.Destroy;
begin
  // Also free's all objects in the list
  FreeAndNil(FChildFactories);
  inherited;
end;

// Add the given object and take ownership over it.
procedure TSerializableCompositeFactory.Add(o : TSerializableFactory);
begin
  if Assigned(o) then FChildFactories.Add(o);
end;

// Remove the given object from the factory and ownership are returned to the caller.
function TSerializableCompositeFactory.Remove(o : TSerializableFactory) : Boolean;
var
  Idx : Integer;
begin
  Result := False;
  Idx := FChildFactories.IndexOf(o);
  if Idx >= 0 then begin
    // Remove from list without freeing object
    FChildFactories.Extract(o);
    Result := True;
  end;
end;

function TSerializableCompositeFactory.Make(types : string) : TSerializable;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to FChildFactories.Count - 1 do begin
    Result := FChildFactories[i].Make(types);
    if Assigned(Result) then Break;
	end;
end;

end.


