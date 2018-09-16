--
-- Copyright (C) 2018 Lennart Andersson.
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

with Ada.Real_Time.Timing_Events;
with Ada.Containers.Vectors;

generic
  MinCapacity : Positive;
package Ops_Pa.DeadlineNotifier_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type DeadlineListener_Interface is limited interface;
  type DeadlineListener_Interface_At is access all DeadlineListener_Interface'Class;

  -- Override this to react on the notification callback
  procedure OnDeadlineMissed( Self : in out DeadlineListener_Interface; Sender : in Ops_Class_At ) is abstract;


-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type DeadlineNotifier_Class    is new Ops_Class with private;
  type DeadlineNotifier_Class_At is access all DeadlineNotifier_Class'Class;

  function Create( Owner : Ops_Class_At; Periodic : Boolean := False ) return DeadlineNotifier_Class_At;

  procedure Start( Self : in out DeadlineNotifier_Class; millis : TimeMs_T );
  procedure Cancel( Self : in out DeadlineNotifier_Class );

  type OnNotifyEvent_T is access procedure( Sender : in out Ops_Class_At; Arg : Ops_Class_At );

  -- Register a Listener for callback via a procedure call
  procedure addListener( Self  : in out DeadlineNotifier_Class;
                         Proc  : in OnNotifyEvent_T;
                         Arg   : in Ops_Class_At );

  procedure removeListener( Self  : in out DeadlineNotifier_Class;
                            Proc  : in OnNotifyEvent_T;
                            Arg   : in Ops_Class_At );

  -- Register a Listener for callback using a "listener" class
  procedure addListener( Self     : in out DeadlineNotifier_Class;
                         Listener : in DeadlineListener_Interface_At );

  procedure removeListener( Self     : in out DeadlineNotifier_Class;
                            Listener : in DeadlineListener_Interface_At );

  function numListeners(Self : in out DeadlineNotifier_Class) return Integer;

private
  type Listener_T is record
    Proc    : OnNotifyEvent_T := null;
    Arg     : Ops_Class_At := null;
    ClassAt : DeadlineListener_Interface_At := null;
  end record;

  function Equal( Left, Right : Listener_T ) return Boolean;

  subtype MyIndex_T is Integer range 0..Integer'Last;
  package MyVector_Pa is new Ada.Containers.Vectors(MyIndex_T, Listener_T, Equal);

  type Internal_Event is new Ada.Real_Time.Timing_Events.Timing_Event with
    record
      Notifier : DeadlineNotifier_Class_At := null;
    end record;

  type DeadlineNotifier_Class is new Ops_Class with
    record
      Owner        : Ops_Class_At := null;
      Listeners    : MyVector_Pa.Vector;
      Event        : Internal_Event;
      Timeout      : TimeMs_T := 0;
      Periodic     : Boolean := False;
    end record;

  procedure InitInstance( Self     : in out DeadlineNotifier_Class;
                          SelfAt   : DeadlineNotifier_Class_At;
                          Owner    : Ops_Class_At;
                          Periodic : Boolean );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out DeadlineNotifier_Class );

end Ops_Pa.DeadlineNotifier_Pa;

