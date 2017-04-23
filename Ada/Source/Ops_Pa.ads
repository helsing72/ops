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

with System,
     Ada.Unchecked_Deallocation,
     Ada.Exceptions,
     Com_Base_Abs_Pa,
     Ctv;

use  System;

package Ops_Pa is

  -- Base Class for all OPS classes
  type Ops_Class    is abstract new Com_Base_Abs_Pa.Com_Base_Abs_Class with null record;
  type Ops_Class_At is access all Ops_Class'Class;

  -- Extra parameter in notification callbacks using a procedure call
  subtype NotifyParam_T is Com_Base_Abs_Pa.Com_Base_Abs_Class_At;

  -- Types
  subtype Byte    is Ctv.Unsigned_Int8;
  subtype UInt8   is Ctv.Unsigned_Int8;
  subtype UInt16  is Ctv.Unsigned_Int16;
  subtype UInt32  is Ctv.Unsigned_Int32;
  subtype UInt64  is Ctv.Unsigned_Int64;

  subtype Int8    is Ctv.Integer8;
  subtype Int16   is Ctv.Integer16;
  subtype Int32   is Ctv.Integer32;
  subtype Int64   is Ctv.Integer64;

  subtype Float32 is Ctv.Float32;
  subtype Float64 is Ctv.Float64;

  type String_At  is access all String;

  -- Types used for implementing OPS static and dynamic arrays
  type Boolean_Arr is array(Integer range <>) of Boolean;
  type Boolean_Arr_At is access all Boolean_Arr;
  type Byte_Arr is array(Integer range <>) of aliased Byte;
  type Byte_Arr_At is access all Byte_Arr;
  type Int16_Arr is array(Integer range <>) of Int16;
  type Int16_arr_At is access all Int16_Arr;
  type Int32_Arr is array(Integer range <>) of Int32;
  type Int32_Arr_At is access all Int32_Arr;
  type Int64_Arr is array(Integer range <>) of Int64;
  type Int64_Arr_At is access all Int64_Arr;
  type Float32_Arr is array(Integer range <>) of Float32;
  type Float32_Arr_At is access all Float32_Arr;
  type Float64_Arr is array(Integer range <>) of Float64;
  type Float64_Arr_At is access all Float64_Arr;
  type String_Arr is array(Integer range <>) of String_At;
  type String_Arr_At is access all String_Arr;

  -- Access to operations on types
  use type Ctv.Unsigned_Int8;
  use type Ctv.Unsigned_Int16;
  use type Ctv.Unsigned_Int32;
  use type Ctv.Unsigned_Int64;

  use type Ctv.Integer8;
  use type Ctv.Integer16;
  use type Ctv.Integer32;
  use type Ctv.Integer64;

  use type Ctv.Float32;
  use type Ctv.Float64;

  -- Constants
  PACKET_MAX_SIZE : constant := 60000;
  MAX_DEADLINE_TIMEOUT : constant := Int64'Last;
--  Is_Network_Byte_Order : constant Boolean := System.Default_Bit_Order = System.High_Order_First;

  -- Exceptions
  Not_Yet_Implemented : exception;

  EConfigException : exception;
  ENoSuchTopicException : exception;
  ECommException : exception;
  --  EReceiveTimedOutException : Exception;
  Illegal_Array_Length : exception;
  Null_Object_In_Array : exception;
  Null_Object_Not_Allowed : exception;

  -- String helpers
  function Copy( Str : String ) return String_At;
  function Copy( Str : String_At ) return String_At;
  procedure Dispose is new Ada.Unchecked_Deallocation( String, String_At );
  procedure Replace( Str_At : in out String_At; Str : String );
  procedure Replace( Str_At : in out String_At; Str : String_At );

  -- String Array helpers
  procedure Clear( Arr : in out String_Arr );
  procedure Clear( Arr : in out String_Arr_At );

  -- Time helpers
  subtype TimeMs_T is UInt32;
  function GetTimeInMs return TimeMs_T;

  -- Dispose routines for our dynamic arrays
  procedure Dispose is new Ada.Unchecked_Deallocation( Boolean_Arr, Boolean_Arr_At );
  procedure Dispose is new Ada.Unchecked_Deallocation( Byte_Arr, Byte_Arr_At );
  procedure Dispose is new Ada.Unchecked_Deallocation( Int16_Arr, Int16_Arr_At );
  procedure Dispose is new Ada.Unchecked_Deallocation( Int32_Arr, Int32_Arr_At );
  procedure Dispose is new Ada.Unchecked_Deallocation( Int64_Arr, Int64_Arr_At );
  procedure Dispose is new Ada.Unchecked_Deallocation( Float32_Arr, Float32_Arr_At );
  procedure Dispose is new Ada.Unchecked_Deallocation( Float64_Arr, Float64_Arr_At );
  procedure Dispose is new Ada.Unchecked_Deallocation( String_Arr, String_Arr_At );

  -- Debug
  procedure Trace( NameStr, ValueStr : String );
  procedure Trace( NameStr, ValueStr : String; E : Ada.Exceptions.Exception_Occurrence );

  function ToString(ptr : Byte_Arr_At) return String;

end Ops_Pa;

