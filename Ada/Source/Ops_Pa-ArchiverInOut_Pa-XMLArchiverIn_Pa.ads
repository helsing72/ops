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

with DOM.Readers;
with Dom.Core;

with Ops_Pa.SerializableFactory_Pa.CompFactory_Pa;
use  Ops_Pa.SerializableFactory_Pa.CompFactory_Pa;

package Ops_Pa.ArchiverInOut_Pa.XMLArchiverIn_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type XMLArchiverIn_Class    is new ArchiverInOut_Class with private;
  type XMLArchiverIn_Class_At is access all XMLArchiverIn_Class'Class;

  -- Constructors
  function Create( xmlString : string; topNode : string; fact : SerializableInheritingTypeFactory_Class_At ) return XMLArchiverIn_Class_At;

  overriding function IsOut( Self : in XMLArchiverIn_Class) return Boolean;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Boolean);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Byte);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int32);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int16);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int64);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float32);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float64);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out String_At);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Serializable_Class_At);

  overriding function inout2( Self : in out XMLArchiverIn_Class; name : String; value : in out Serializable_Class_At) return Serializable_Class_At;

  overriding function inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Serializable_Class_At; element : Integer) return Serializable_Class_At;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Boolean_Arr_At);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Byte_Arr_At);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int32_Arr_At);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int16_Arr_At);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int64_Arr_At);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float32_Arr_At);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float64_Arr_At);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out String_Arr_At);

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Boolean_Arr);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Byte_Arr);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int32_Arr);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int16_Arr);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int64_Arr);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float32_Arr);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float64_Arr);
  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out String_Arr);

--  procedure inout( Self : in out XMLArchiverIn_Class; name : string; Value : in out Serializable_Class_Arr_At);

  overriding function beginList( Self : in out XMLArchiverIn_Class; name : String; size : Integer) return Integer;
  overriding procedure endList( Self : in out XMLArchiverIn_Class; name : String);

private
-- ==========================================================================
--
-- ==========================================================================
  type XMLArchiverIn_Class is new ArchiverInOut_Class with
    record
      SelfAt : XMLArchiverIn_Class_At := null;
      Factory : SerializableInheritingTypeFactory_Class_At := null;
      Reader : DOM.Readers.Tree_Reader;
      Doc : DOM.Core.Document := null;
      CurrentNode : DOM.Core.Node := null;
    end record;

  procedure InitInstance( Self : in out XMLArchiverIn_Class;
                          SelfAt : XMLArchiverIn_Class_At;
                          xmlString : String;
                          topNode : String;
                          fact : SerializableInheritingTypeFactory_Class_At);

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out XMLArchiverIn_Class );

end Ops_Pa.ArchiverInOut_Pa.XMLArchiverIn_Pa;

