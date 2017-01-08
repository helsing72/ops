--
-- Copyright (C) 2016 Lennart Andersson.
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

with Ops_Pa.ByteBuffer_Pa;
use Ops_Pa.ByteBuffer_Pa;
with Ops_Pa.SerializableFactory_Pa.CompFactory_Pa;
use Ops_Pa.SerializableFactory_Pa.CompFactory_Pa;

package Ops_Pa.ArchiverInOut_Pa.ArchiverIn_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type ArchiverIn_Class    is new ArchiverInOut_Class with private;
  type ArchiverIn_Class_At is access all ArchiverIn_Class'Class;

  -- Constructors
  function Create( buf : ByteBuffer_Class_At; fact : SerializableInheritingTypeFactory_Class_At ) return ArchiverIn_Class_At;

  overriding function IsOut( Self : in ArchiverIn_Class) return Boolean;

  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Boolean);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Byte);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Int32);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Int16);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Int64);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Float32);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Float64);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out String_At);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Serializable_Class_At);

  function inout2( Self : in out ArchiverIn_Class; name : String; value : in out Serializable_Class_At) return Serializable_Class_At;

  function inout( Self : in out ArchiverIn_Class; name : String; value : in out Serializable_Class_At; element : Integer) return Serializable_Class_At;

  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Boolean_Arr_At);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Byte_Arr_At);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Int32_Arr_At);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Int16_Arr_At);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Int64_Arr_At);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Float32_Arr_At);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Float64_Arr_At);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out String_Arr_At);

  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Boolean_Arr);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Byte_Arr);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Int32_Arr);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Int16_Arr);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Int64_Arr);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Float32_Arr);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out Float64_Arr);
  procedure inout( Self : in out ArchiverIn_Class; name : String; value : in out String_Arr);

--  procedure inout( Self : in out ArchiverIn_Class; name : string; Value : in out Serializable_Class_Arr_At);

  function beginList( Self : in out ArchiverIn_Class; name : String; size : Integer) return Integer;
  procedure endList( Self : in out ArchiverIn_Class; name : String);

private
-- ==========================================================================
--
-- ==========================================================================
  type ArchiverIn_Class is new ArchiverInOut_Class with
    record
      SelfAt : ArchiverIn_Class_At := null;
      FBuf : ByteBuffer_Class_At := null;
      Factory : SerializableInheritingTypeFactory_Class_At := null;
    end record;

  procedure InitInstance( Self : in out ArchiverIn_Class;
                          SelfAt : ArchiverIn_Class_At;
                          buf : ByteBuffer_Class_At;
                          fact : SerializableInheritingTypeFactory_Class_At);

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out ArchiverIn_Class );

end Ops_Pa.ArchiverInOut_Pa.ArchiverIn_Pa;
