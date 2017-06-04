--
-- Copyright (C) 2016-2017 Lennart Andersson.
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

with Ada.Unchecked_Deallocation;

package Ops_Pa.ArchiverInOut_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type ArchiverInOut_Class    is abstract new Ops_Class with null record;
  type ArchiverInOut_Class_At is access all ArchiverInOut_Class'Class;

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type Serializable_Class    is abstract new Ops_Class with null record;
  type Serializable_Class_At is access all Serializable_Class'Class;

  procedure Serialize( Self : in out Serializable_Class; archiver : ArchiverInOut_Class_At ) is abstract;
  function TypesString( Self : Serializable_Class ) return String is abstract;

  --
  function IsOut( Self : in ArchiverInOut_Class) return Boolean is abstract;

  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Boolean) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Byte) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Int32) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Int16) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Int64) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Float32) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Float64) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out String_At) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Serializable_Class_At) is abstract;

  function inout2( Self : in out ArchiverInOut_Class; name : String; value : in out Serializable_Class_At) return Serializable_Class_At is abstract;

  function inout( Self : in out ArchiverInOut_Class; name : String; value : in out Serializable_Class_At; element : Integer) return Serializable_Class_At is abstract;

  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Boolean_Arr_At) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Byte_Arr_At) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Int32_Arr_At) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Int16_Arr_At) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Int64_Arr_At) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Float32_Arr_At) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Float64_Arr_At) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out String_Arr_At) is abstract;

  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Boolean_Arr) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Byte_Arr) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Int32_Arr) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Int16_Arr) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Int64_Arr) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Float32_Arr) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out Float64_Arr) is abstract;
  procedure inout( Self : in out ArchiverInOut_Class; name : String; value : in out String_Arr) is abstract;

  type Serializable_Class_Arr is array(Integer range <>) of Serializable_Class_At;
  type Serializable_Class_Arr_At is access all Serializable_Class_Arr;

  procedure Dispose is new Ada.Unchecked_Deallocation( Serializable_Class_Arr, Serializable_Class_Arr_At );

--  procedure inout( Self : in out ArchiverInOut_Class; name : string; Value : in out Serializable_Class_Arr_At) is abstract;

  function beginList( Self : in out ArchiverInOut_Class; name : String; size : Integer) return Integer is abstract;
  procedure endList( Self : in out ArchiverInOut_Class; name : String) is abstract;

  generic
    type Item is new Serializable_Class with private;
    type Item_At is access all Item'Class;
    type Item_Arr is array(Integer range <>) of aliased Item;
  procedure inoutfixarr(archiver : ArchiverInOut_Class_At; name : string; value : in out Item_Arr);

  generic
    type Item is new Serializable_Class with private;
    type Item_At is access all Item'Class;
    type Item_Arr is array(Integer range <>) of aliased Item;
    type Item_Arr_At is access all Item_Arr;
  procedure inoutdynarr(archiver : ArchiverInOut_Class_At; name : string; value : in out Item_Arr_At);

  generic
    type Item is new Serializable_Class with private;
    type Item_At is access all Item'Class;
    type Item_At_Arr is array(Integer range <>) of Item_At;
  procedure inoutfixarr2(archiver : ArchiverInOut_Class_At; name : string; value : in out Item_At_Arr);

  generic
    type Item is new Serializable_Class with private;
    type Item_At is access all Item'Class;
    type Item_At_Arr is array(Integer range <>) of Item_At;
    type Item_At_Arr_At is access all Item_At_Arr;
  procedure inoutdynarr2(archiver : ArchiverInOut_Class_At; name : string; value : in out Item_At_Arr_At);

private
-- ==========================================================================
--
-- ==========================================================================
  procedure SetTypesString( Self : in out Serializable_Class; types : String );

  procedure SetTypesString( Self : in out ArchiverInOut_Class; obj : Serializable_Class_At; types : String);

end Ops_Pa.ArchiverInOut_Pa;
