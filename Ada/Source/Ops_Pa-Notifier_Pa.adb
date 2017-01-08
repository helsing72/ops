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

package body Ops_Pa.Notifier_Pa is

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function Create( Owner : Ops_Class_At ) return Notifier_Class_At is
    Self : Notifier_Class_At := null;
  begin
    Self := new Notifier_Class;
    InitInstance( Self.all, Owner );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  --------------------------------------------------------------------------
  -- Called by "owner" that wishes to notify its listeners.
  --------------------------------------------------------------------------
  procedure doNotify( Self : in out Notifier_Class; Item : in Item_T ) is
  begin
    for i in 1..Self.NumListeners loop
      if Self.Listeners(i).ClassAt /= null then
        OnNotify( Self.Listeners(i).ClassAt.all, Self.Owner, Item );
      else
        Self.Listeners(i).Proc.all( Self.Owner, Item );
      end if;
    end loop;
  end;

  --------------------------------------------------------------------------
  -- Register a Listener for callback via a procedure call
  --------------------------------------------------------------------------
  procedure addListener( Self  : in out Notifier_Class; 
                         proc  : in OnNotifyEvent_T ) is
  begin
    if Self.NumListeners < MaxListeners then
      Self.NumListeners := Self.NumListeners + 1;
      Self.Listeners(Self.NumListeners).Proc := Proc;
    else
      raise ETooManyListeners;
    end if;
  end;

  --------------------------------------------------------------------------
  -- Register a Listener for callback using a "listener" class
  --------------------------------------------------------------------------
  procedure addListener( Self     : in out Notifier_Class; 
                         Listener : in Listener_Class_At) is
  begin
    if Self.NumListeners < MaxListeners then
      Self.NumListeners := Self.NumListeners + 1;
      Self.Listeners(Self.NumListeners).ClassAt := Listener;
    else
      raise ETooManyListeners;
    end if;
  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure InitInstance( Self : in out Notifier_Class; Owner : Ops_Class_At ) is
  begin
    Self.Owner := Owner;
  end InitInstance;

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out Notifier_Class ) is
  begin
    null;
  end Finalize;

end Ops_Pa.Notifier_Pa;

