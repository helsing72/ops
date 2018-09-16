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

package body Ops_Pa.DeadlineNotifier_Pa is

  use type MyVector_Pa.Cursor;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function Equal( Left, Right : Listener_T ) return Boolean is
  begin
    return Left.Proc = Right.Proc and
      Left.Arg = Right.Arg and
      Left.ClassAt = Right.ClassAt;
  end;

  --------------------------------------------------------------------------
  -- Use a protected type to synchronise between the callback (that need to call
  -- a protected procedure) and the adding/removing of listeners.
  --------------------------------------------------------------------------
  protected Event_Receiver is

    procedure Handler(Event : in out Ada.Real_Time.Timing_Events.Timing_Event);

    procedure doNotify(Notifier : in out DeadlineNotifier_Class_At);

    procedure addListener( Notifier : in out DeadlineNotifier_Class;
                           Proc     : in OnNotifyEvent_T;
                           Arg      : in Ops_Class_At );

    procedure removeListener( Notifier : in out DeadlineNotifier_Class;
                              Proc     : in OnNotifyEvent_T;
                              Arg      : in Ops_Class_At );

    procedure addListener( Notifier : in out DeadlineNotifier_Class;
                           Listener : in DeadlineListener_Interface_At);

    procedure removeListener( Notifier : in out DeadlineNotifier_Class;
                              Listener : in DeadlineListener_Interface_At );

  end;

  protected body Event_Receiver is

    ------------------------------------------------------------------------
    procedure Handler(Event : in out Ada.Real_Time.Timing_Events.Timing_Event) is
      Notifier : DeadlineNotifier_Class_At := null;
    begin
      Notifier := Internal_Event(Ada.Real_Time.Timing_Events.Timing_Event'Class(Event)).Notifier;
      if Notifier /= null then
        doNotify( Notifier );
      end if;
    end;

    ------------------------------------------------------------------------
    procedure doNotify(Notifier : in out DeadlineNotifier_Class_At) is
    begin
      if Notifier.Periodic then
        Notifier.Start( Notifier.Timeout );
      end if;
      for i in Notifier.Listeners.First_Index .. Notifier.Listeners.Last_Index loop
        -- We don't allow a client to stop us from notify all clients
        begin
          if Notifier.Listeners.Element(i).ClassAt /= null then
            OnDeadlineMissed( Notifier.Listeners.Element(i).ClassAt.all, Notifier.Owner );
          else
            Notifier.Listeners.Element(i).Proc.all( Notifier.Owner, Notifier.Listeners.Element(i).Arg );
          end if;
        exception
          when others =>
            null;
        end;
      end loop;
    end;

    ------------------------------------------------------------------------
    procedure addListener( Notifier : in out DeadlineNotifier_Class;
                           Proc     : in OnNotifyEvent_T;
                           Arg      : in Ops_Class_At ) is
    begin
      Notifier.Listeners.Append(Listener_T'(Proc => Proc, Arg => Arg, ClassAt => null));
    end;

    ------------------------------------------------------------------------
    procedure removeListener( Notifier : in out DeadlineNotifier_Class;
                              Proc     : in OnNotifyEvent_T;
                              Arg      : in Ops_Class_At ) is
      Cursor : MyVector_Pa.Cursor;
    begin
      Cursor := Notifier.Listeners.Find(Listener_T'(Proc => Proc, Arg => Arg, ClassAt => null));
      if Cursor /= MyVector_Pa.No_Element then
        Notifier.Listeners.Delete(Cursor);
      end if;
    end;

    ------------------------------------------------------------------------
    procedure addListener( Notifier : in out DeadlineNotifier_Class;
                           Listener : in DeadlineListener_Interface_At) is
    begin
      Notifier.Listeners.Append(Listener_T'(Proc => null, Arg => null, ClassAt => Listener));
    end;

    ------------------------------------------------------------------------
    procedure removeListener( Notifier : in out DeadlineNotifier_Class;
                              Listener : in DeadlineListener_Interface_At ) is
      Cursor : MyVector_Pa.Cursor;
    begin
      Cursor := Notifier.Listeners.Find(Listener_T'(Proc => null, Arg => null, ClassAt => Listener));
      if Cursor /= MyVector_Pa.No_Element then
        Notifier.Listeners.Delete(Cursor);
      end if;
    end;

  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function Create( Owner : Ops_Class_At; Periodic : Boolean := False ) return DeadlineNotifier_Class_At is
    Self : DeadlineNotifier_Class_At := null;
  begin
    Self := new DeadlineNotifier_Class;
    InitInstance( Self.all, Self, Owner, Periodic );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure Start( Self : in out DeadlineNotifier_Class; millis : TimeMs_T ) is
  begin
    if millis > 0 then
      Self.Timeout := millis;
      Self.Event.Set_Handler( Ada.Real_Time.Milliseconds( Integer(millis) ), Event_Receiver.Handler'Access);
    else
      Self.Cancel;
    end if;
  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure Cancel( Self : in out DeadlineNotifier_Class ) is
    Cancelled : Boolean;
  begin
    Self.Timeout := 0;
    Self.Event.Cancel_Handler( Cancelled );
  end;

  --------------------------------------------------------------------------
  -- Register a Listener for callback via a procedure call
  --------------------------------------------------------------------------
  procedure addListener( Self  : in out DeadlineNotifier_Class;
                         Proc  : in OnNotifyEvent_T;
                         Arg   : in Ops_Class_At ) is
  begin
    if Proc /= null then
      Event_Receiver.addListener( Self, Proc, Arg );
    end if;
  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure removeListener( Self  : in out DeadlineNotifier_Class;
                            Proc  : in OnNotifyEvent_T;
                            Arg   : in Ops_Class_At ) is
  begin
    if Proc /= null then
      Event_Receiver.removeListener( Self, Proc, Arg );
    end if;
  end;

  --------------------------------------------------------------------------
  -- Register a Listener for callback using a "listener" class
  --------------------------------------------------------------------------
  procedure addListener( Self     : in out DeadlineNotifier_Class;
                         Listener : in DeadlineListener_Interface_At) is
  begin
    if Listener /= null then
      Event_Receiver.addListener( Self, Listener );
    end if;
  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure removeListener( Self     : in out DeadlineNotifier_Class;
                            Listener : in DeadlineListener_Interface_At ) is
  begin
    if Listener /= null then
      Event_Receiver.removeListener( Self, Listener );
    end if;
  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function numListeners(Self : in out DeadlineNotifier_Class) return Integer is
  begin
    return Integer(Self.Listeners.Length);
  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure InitInstance( Self     : in out DeadlineNotifier_Class;
                          SelfAt   : DeadlineNotifier_Class_At;
                          Owner    : Ops_Class_At;
                          Periodic : Boolean ) is
  begin
    Self.Event.Notifier := SelfAt;
    Self.Owner := Owner;
    Self.Periodic := Periodic;
    Self.Listeners.Reserve_Capacity(Ada.Containers.Count_Type(MinCapacity));
  end InitInstance;

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out DeadlineNotifier_Class ) is
  begin
    Self.Cancel;
  end Finalize;

end Ops_Pa.DeadlineNotifier_Pa;

