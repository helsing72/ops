unit uOps.SerializableInheritingTypeFactory;

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

uses uOps.ArchiverInOut,
     uOps.SerializableCompositeFactory;

type
  TSerializableInheritingTypeFactory = class(TSerializableCompositeFactory)
  public
    // Tries to construct the most specialized object in the given typeString list
    function Make(types : string) : TSerializable; override;
  end;

implementation

uses System.Types,
     System.StrUtils;

// Tries to construct the most specialized object in the given typeString list
function TSerializableInheritingTypeFactory.Make(types : string) : TSerializable;
var
  i : Integer;
  vtypes: System.Types.TStringDynArray;
begin
  // types: MostSpecializedType MoreSpecializedType DerivedType ... BaseType
  vtypes := SplitString(types, ' ');

  Result := nil;
  for i := 0 to Length(vtypes) - 1 do begin
    Result := inherited Make(vtypes[i]);
		if Assigned(Result) then Break;
	end;
end;

end.

