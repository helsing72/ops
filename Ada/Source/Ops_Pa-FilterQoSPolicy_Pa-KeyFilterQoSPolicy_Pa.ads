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

with Ops_Pa.OpsObject_Pa;

use  Ops_Pa.OpsObject_Pa;

package Ops_Pa.FilterQoSPolicy_Pa.KeyFilterQoSPolicy_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type KeyFilterQoSPolicy_Class    is new FilterQoSPolicy_Class with private;
  type KeyFilterQoSPolicy_Class_At is access all KeyFilterQoSPolicy_Class'Class;

  -- Creates a filter that as default lets all objects thru.
  -- If a key is given, it must match for objects to come thru.
  function Create( Max_Keys : Integer := 10; key : String := "" ) return KeyFilterQoSPolicy_Class_At;

  -- Add more keys, if any key matches, the object comes thru
  procedure AddKey( Self : in out KeyFilterQoSPolicy_Class; key : String );

  -- Clear all keys making an empty filter that lets all objects thru
  procedure Clear( Self : in out KeyFilterQoSPolicy_Class);

  -- Overides applyFilter(OPSObject* o) in FilterQoSPolicy
  overriding function ApplyFilter( Self : KeyFilterQoSPolicy_Class; o : OpsObject_Class_At) return Boolean;

  TooManyKeys : exception;

private
-- ==========================================================================
--
-- ==========================================================================
  protected type KeyStorage_T is

    procedure Init( Max : Integer );
    procedure Dispose;

    procedure Add( Key : String );
    procedure Clear;
    function Check( o : OpsObject_Class_At ) return Boolean;

  private
    Keys : String_Arr_At := null;
    Max_Keys : Integer := 0;
    Num_Keys : Integer := 0;
  end KeyStorage_T;

  type KeyFilterQoSPolicy_Class is new FilterQoSPolicy_Class with
    record
      Keys : KeyStorage_T;
    end record;

  procedure InitInstance( Self : in out KeyFilterQoSPolicy_Class;  Max_Keys : Integer; key : String );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out KeyFilterQoSPolicy_Class );

end Ops_Pa.FilterQoSPolicy_Pa.KeyFilterQoSPolicy_Pa;

