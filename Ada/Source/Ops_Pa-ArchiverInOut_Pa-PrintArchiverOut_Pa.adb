--
-- Copyright (C) 2017 Lennart Andersson.
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

with Ada.Characters.Latin_1;
with Ada.Text_IO;

with Ops_Pa.OpsObject_Pa;

use Ops_Pa.OpsObject_Pa;

package body Ops_Pa.ArchiverInOut_Pa.PrintArchiverOut_Pa is

  cEndl : constant String := Ada.Characters.Latin_1.CR & Ada.Characters.Latin_1.LF;

  procedure Add( Self : in out PrintArchiverOut_Class; str : String ) is
  begin
    Ada.Text_IO.Put( Tab(Self) & str & cEndl );
  end;

  function Tab( Self : PrintArchiverOut_Class ) return String is

    function LTab( depth : Integer ) return String is
      t : constant String := "   ";
    begin
      if depth = 1 then
        return t;
      else
        return LTab( depth - 1 ) & t;
      end if;
    end;

  begin
    if Self.CurrentTabDepth > 0 then
      return LTab( Self.CurrentTabDepth );
    else
      return "";
    end if;
  end;

  function StringValue(value : String_At) return String is
  begin
    if value /= null then
      return value.all;
    else
      return "(null)";
    end if;
  end;


  function Create return PrintArchiverOut_Class_At is
    Self : PrintArchiverOut_Class_At := null;
  begin
    Self := new PrintArchiverOut_Class;
    InitInstance( Self.all, Self );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  procedure InitInstance( Self : in out PrintArchiverOut_Class; SelfAt : PrintArchiverOut_Class_At ) is
  begin
    Self.SelfAt := SelfAt;
  end;

  overriding procedure Finalize( Self : in out PrintArchiverOut_Class ) is
  begin
    null;
  end;

  function IsOut( Self : in PrintArchiverOut_Class) return Boolean is
  begin
    return True;
  end;

  procedure PrintObject( Self : in out PrintArchiverOut_Class; name : string; obj : in out Serializable_Class_At ) is
  begin
    Self.Add(cEndl & "________________Begin Object___________________" & cEndl & cEndl);
    Self.inout(name, obj);
    Self.Add(cEndl & "_________________End Object____________________" & cEndl);
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Boolean) is
  begin
    Self.Add(name & " = " & Boolean'Image(value));
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Byte) is
  begin
    Self.Add(name & " = " & Byte'Image(value));
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Int32) is
  begin
    Self.Add(name & " = " & Int32'Image(value));
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Int16) is
  begin
    Self.Add(name & " = " & Int16'Image(value));
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Int64) is
  begin
    Self.Add(name & " = " & Int64'Image(value));
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Float32) is
  begin
    Self.Add(name & " = " & Float32'Image(value));
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Float64) is
  begin
    Self.Add(name & " = " & Float64'Image(value));
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out String_At) is
  begin
    Self.Add(name & " = " & StringValue(value));
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Serializable_Class_At) is
    Obj : OpsObject_Class_At;
  begin
    if value = null then
      raise Null_Object_Not_Allowed;
    end if;
    if value.all in OpsObject_Class'Class then
      Obj := OpsObject_Class_At(value);
      Self.Add(name & " type = '" & Obj.TypesString & "'");
      Self.CurrentTabDepth := Self.CurrentTabDepth + 1;
      value.Serialize(ArchiverInOut_Class_At(Self.SelfAt));
      Self.CurrentTabDepth := Self.CurrentTabDepth - 1;
    end if;
  end;

  function inout2( Self : in out PrintArchiverOut_Class; name : String; value : in out Serializable_Class_At) return Serializable_Class_At is
    Obj : OpsObject_Class_At;
  begin
    if value = null then
      raise Null_Object_Not_Allowed;
    end if;
    if value.all in OpsObject_Class'Class then
      Obj := OpsObject_Class_At(value);
      Self.Add(name & " type = '" & string(Obj.TypesString) & "'");
      Self.CurrentTabDepth := Self.CurrentTabDepth + 1;
      value.Serialize(ArchiverInOut_Class_At(Self.SelfAt));
      Self.CurrentTabDepth := Self.CurrentTabDepth - 1;
    end if;
    return value;
  end;

  function inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Serializable_Class_At; element : Integer) return Serializable_Class_At is
    Obj : OpsObject_Class_At;
  begin
    if value = null then
      raise Null_Object_Not_Allowed;
    end if;
    if value.all in OpsObject_Class'Class then
      Obj := OpsObject_Class_At(value);
      Self.Add(name & " type = '" & string(Obj.TypesString) & "'");
      Self.CurrentTabDepth := Self.CurrentTabDepth + 1;
      value.Serialize(ArchiverInOut_Class_At(Self.SelfAt));
      Self.CurrentTabDepth := Self.CurrentTabDepth - 1;
    end if;
    return value;
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Boolean_Arr_At) is
  begin
    inout(Self, name, value.all);
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Boolean_Arr) is
  begin
    if value'Length > 0 then
      declare
        val0 : String := Boolean'Image(value(value'First));
        valx : String := Boolean'Image(value(value'Last));
      begin
        Self.Add(name & "(size = " & Integer'Image(value'Length) & ") = [" & val0 & "..." & valx & "]");
      end;
    else
      Self.Add(name & "(size = " & Integer'Image(value'Length) & ")");
    end if;
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Byte_Arr_At) is
  begin
    inout(Self, name, value.all);
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Byte_Arr) is
  begin
    if value'Length > 0 then
      declare
        val0 : String := Byte'Image(value(value'First));
        valx : String := Byte'Image(value(value'Last));
      begin
        Self.Add(name & "(size = " & Integer'Image(value'Length) & ") = [" & val0 & "..." & valx & "]");
      end;
    else
      Self.Add(name & "(size = " & Integer'Image(value'Length) & ")");
    end if;
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Int32_Arr_At) is
  begin
    inout(Self, name, value.all);
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Int32_Arr) is
  begin
    if value'Length > 0 then
      declare
        val0 : String := Int32'Image(value(value'First));
        valx : String := Int32'Image(value(value'Last));
      begin
        Self.Add(name & "(size = " & Integer'Image(value'Length) & ") = [" & val0 & "..." & valx & "]");
      end;
    else
      Self.Add(name & "(size = " & Integer'Image(value'Length) & ")");
    end if;
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Int16_Arr_At) is
  begin
    inout(Self, name, value.all);
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Int16_Arr) is
  begin
    if value'Length > 0 then
      declare
        val0 : String := Int16'Image(value(value'First));
        valx : String := Int16'Image(value(value'Last));
      begin
        Self.Add(name & "(size = " & Integer'Image(value'Length) & ") = [" & val0 & "..." & valx & "]");
      end;
    else
      Self.Add(name & "(size = " & Integer'Image(value'Length) & ")");
    end if;
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Int64_Arr_At) is
  begin
    inout(Self, name, value.all);
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Int64_Arr) is
  begin
    if value'Length > 0 then
      declare
        val0 : String := Int64'Image(value(value'First));
        valx : String := Int64'Image(value(value'Last));
      begin
        Self.Add(name & "(size = " & Integer'Image(value'Length) & ") = [" & val0 & "..." & valx & "]");
      end;
    else
      Self.Add(name & "(size = " & Integer'Image(value'Length) & ")");
    end if;
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Float32_Arr_At) is
  begin
    inout(Self, name, value.all);
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Float32_Arr) is
  begin
    if value'Length > 0 then
      declare
        val0 : String := Float32'Image(value(value'First));
        valx : String := Float32'Image(value(value'Last));
      begin
        Self.Add(name & "(size = " & Integer'Image(value'Length) & ") = [" & val0 & "..." & valx & "]");
      end;
    else
      Self.Add(name & "(size = " & Integer'Image(value'Length) & ")");
    end if;
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Float64_Arr_At) is
  begin
    inout(Self, name, value.all);
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out Float64_Arr) is
  begin
    if value'Length > 0 then
      declare
        val0 : String := Float64'Image(value(value'First));
        valx : String := Float64'Image(value(value'Last));
      begin
        Self.Add(name & "(size = " & Integer'Image(value'Length) & ") = [" & val0 & "..." & valx & "]");
      end;
    else
      Self.Add(name & "(size = " & Integer'Image(value'Length) & ")");
    end if;
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out String_Arr_At) is
  begin
    inout(Self, name, value.all);
  end;

  procedure inout( Self : in out PrintArchiverOut_Class; name : String; value : in out String_Arr) is
  begin
    if value'Length > 0 then
      declare
        val0 : String := StringValue(value(value'First));
        valx : String := StringValue(value(value'Last));
      begin
        Self.Add(name & "(size = " & Integer'Image(value'Length) & ") = [" & val0 & "..." & valx & "]");
      end;
    else
      Self.Add(name & "(size = " & Integer'Image(value'Length) & ")");
    end if;
  end;

  function beginList( Self : in out PrintArchiverOut_Class; name : String; size : Integer) return Integer is
  begin
    Self.Add(name & " = ");
    Self.CurrentTabDepth := Self.CurrentTabDepth + 1;
    return size;
  end;

  procedure endList( Self : in out PrintArchiverOut_Class; name : String) is
  begin
    Self.CurrentTabDepth := Self.CurrentTabDepth - 1;
  end;

end Ops_Pa.ArchiverInOut_Pa.PrintArchiverOut_Pa;

