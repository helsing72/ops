--
-- Copyright (C) 2016-2019 Lennart Andersson.
--  
-- This file is part of OPS (Open Publish Subscribe).
--  
-- OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--  
-- OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--  
-- You should have received a copy of the GNU Lesser General Public License
-- along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.

package body Ops_Pa.OpsObject_pa is

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function Create return OpsObject_Class_At is
    Self : OpsObject_Class_At := null;
  begin
    Self := new OpsObject_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  overriding procedure Serialize( Self : in out OpsObject_Class; archiver : ArchiverInOut_Class_At) is
  begin
    archiver.inout("key", Self.Key);
  end Serialize;
  
  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function Key( Self : OpsObject_Class ) return String is
  begin
    return Self.Key.all;
  end Key;

  procedure Key( Self : in out OpsObject_Class; Value : String ) is
  begin
    Replace(Self.Key, Value);
  end Key;
  
  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  overriding function TypesString( Self : OpsObject_Class ) return String is
  begin
    return Self.TypesString.all;
  end TypesString;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function SpareBytes( Self : OpsObject_Class ) return Byte_Arr_At is
  begin
    return Self.SpareBytes;
  end SpareBytes;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure SetSpareBytes( Self : in out OpsObject_Class; arr : Byte_Arr_At ) is
  begin
    if Self.SpareBytes /= null then
      Dispose(Self.SpareBytes);
    end if;
    Self.SpareBytes := arr;
  end;
  
  --------------------------------------------------------------------------
  -- Returns a newely allocated deep copy/clone of this object.
  --------------------------------------------------------------------------
  function Clone( Self : OpsObject_Class ) return OpsObject_Class_At is
    Result : OpsObject_Class_At := null;
  begin
    Result := Create;
    Self.FillClone( Result );
    return Result;
  end Clone;
  
  --------------------------------------------------------------------------
  -- Fills the parameter obj with all values from this object.
  --------------------------------------------------------------------------
  procedure FillClone( Self : OpsObject_Class; obj : OpsObject_Class_At ) is
  begin
    Replace(obj.all.Key, Self.Key);
    Replace(obj.all.TypesString, Self.TypesString);
    if Self.SpareBytes /= null then
      obj.all.spareBytes := new Byte_Arr'(Self.SpareBytes.all);
    end if;
  end FillClone;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure AppendType( Self : in out OpsObject_Class; typ : String ) is
  begin
    Replace(Self.TypesString, typ & " " & Self.TypesString.all);
  end AppendType;

  --------------------------------------------------------------------------
  -- Used from archivers
  --------------------------------------------------------------------------
  procedure SetTypesString( Self : in out OpsObject_Class; types : String ) is
  begin
    Replace(Self.TypesString, types);
  end SetTypesString;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure InitInstance( Self : in out OpsObject_Class ) is
  begin
    Self.Key         := Copy("");
    Self.TypesString := Copy("");
  end InitInstance;

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out OpsObject_Class ) is
  begin
    Dispose(Self.Key);
    Dispose(Self.TypesString);
    Dispose(Self.SpareBytes);
  end Finalize;

end Ops_Pa.OpsObject_Pa;

