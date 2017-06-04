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

with Ada.Containers.Vectors;

with Com_Mutex_Pa;

with Ops_Pa.Transport_Pa.Sender_Pa;
use Ops_Pa.Transport_Pa.Sender_Pa;

with Ops_Pa.OpsObject_Pa.Topic_Pa;
use Ops_Pa.OpsObject_Pa.Topic_Pa;

package Ops_Pa.Transport_Pa.SendDataHandler_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type SendDataHandler_Class    is abstract new Ops_Class with private;
  type SendDataHandler_Class_At is access all SendDataHandler_Class'Class;

  function sendData( Self : in out SendDataHandler_Class; buf : Byte_Arr_At; bufSize : Integer; topic : Topic_Class_At) return Boolean is abstract;

  procedure addUser( Self : in out SendDataHandler_Class; client : Ops_Class_At );
  procedure removeUser( Self : in out SendDataHandler_Class; client : Ops_Class_At );

private
-- ==========================================================================
--
-- ==========================================================================
  function Equal( Left, Right : Ops_Class_At ) return Boolean;

  subtype MyIndex_T is Integer range 0..Integer'Last;
  package MyVector_Pa is new Ada.Containers.Vectors(MyIndex_T, Ops_Class_At, Equal);

-- ==========================================================================
--
-- ==========================================================================
  type SendDataHandler_Class is abstract new Ops_Class with
    record
      Users : MyVector_Pa.Vector;

      Sender : Sender_Class_At;
      Mutex : aliased Com_Mutex_Pa.Mutex;
    end record;

  procedure InitInstance( Self : in out SendDataHandler_Class );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out SendDataHandler_Class );

end Ops_Pa.Transport_Pa.SendDataHandler_Pa;

