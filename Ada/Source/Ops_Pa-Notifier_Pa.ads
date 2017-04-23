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

with Com_Mutex_Pa;

generic
  MaxListeners : Positive;
  type Item_T is private;
package Ops_Pa.Notifier_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type Listener_Interface is limited interface;
  type Listener_Interface_At is access all Listener_Interface'Class;

  -- Override this to react on the notification callback
  procedure OnNotify( Self : in out Listener_Interface; Sender : in Ops_Class_At; Item : in Item_T ) is abstract;


-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type Notifier_Class    is new Ops_Class with private;
  type Notifier_Class_At is access all Notifier_Class'Class;

  function Create( Owner : Ops_Class_At ) return Notifier_Class_At;

  -- Called by "Owner" that wishes to notify its listeners.
  procedure doNotify( Self : in out Notifier_Class; Item : in Item_T );

  type OnNotifyEvent_T is access procedure( Sender : in out Ops_Class_At; Item : Item_T; Arg : Ops_Class_At );

  -- Register a Listener for callback via a procedure call
  procedure addListener( Self  : in out Notifier_Class;
                         Proc  : in OnNotifyEvent_T;
                         Arg   : in Ops_Class_At );

  procedure removeListener( Self  : in out Notifier_Class;
                            Proc  : in OnNotifyEvent_T;
                            Arg   : in Ops_Class_At );

  -- Register a Listener for callback using a "listener" class
  procedure addListener( Self     : in out Notifier_Class;
                         Listener : in Listener_Interface_At );

  procedure removeListener( Self     : in out Notifier_Class;
                            Listener : in Listener_Interface_At );

  function numListeners(Self : in out Notifier_Class) return Integer;

  ETooManyListeners : exception;

private
  type Listener_T is record
    Proc    : OnNotifyEvent_T := null;
    Arg     : Ops_Class_At := null;
    ClassAt : Listener_Interface_At := null;
  end record;

  type ListenerArray_T is array (1..MaxListeners) of Listener_T;

  type Notifier_Class is new Ops_Class with
    record
      Owner        : Ops_Class_At := null;
      Mutex        : Com_Mutex_Pa.Mutex;
      Listeners    : ListenerArray_T;
      NumListeners : Integer := 0;
    end record;

  procedure InitInstance( Self : in out Notifier_Class; Owner : Ops_Class_At );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out Notifier_Class );

end Ops_Pa.Notifier_Pa;

