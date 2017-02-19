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

package body Ops_Pa.Transport_Pa.SendDataHandler_Pa is

  use type Ada.Containers.Count_Type;

  procedure addUser( Self : in out SendDataHandler_Class; client : Ops_Class_At ) is
    Idx : MyVector_Pa.Extended_Index;
  begin
    Self.Mutex.Acquire;

    -- Check that it isn't already in the list
    Idx := Self.Users.Find_Index(client);
    if Idx /= MyVector_Pa.No_Index then
      Self.Mutex.Release;
      return;
    end if;

    -- Save client in the list
    Self.Users.Append(client);

    -- For the first client, we open the sender
    if Self.Users.Length = 1 then
      Self.Sender.Open;
    end if;

    Self.Mutex.Release;

  exception
    when others =>
      Self.Mutex.Release;
      raise;
  end;

  procedure removeUser( Self : in out SendDataHandler_Class; client : Ops_Class_At ) is
    Idx : MyVector_Pa.Extended_Index;
  begin
    Self.Mutex.Acquire;

    Idx := Self.Users.Find_Index(client);
    if Idx /= MyVector_Pa.No_Index then
      -- Remove from list without freeing object
      Self.Users.Delete(Idx);
    end if;

    -- For the last client, we close the sender
    if Self.Users.Length = 0 then
      Self.Sender.Close;
    end if;

    Self.Mutex.Release;

  exception
    when others =>
      Self.Mutex.Release;
      raise;
  end;

  function Equal( Left, Right : Ops_Class_At ) return Boolean is
  begin
    return Left = Right;
  end;

  procedure InitInstance( Self : in out SendDataHandler_Class ) is
  begin
    null;
  end;

  procedure Finalize( Self : in out SendDataHandler_Class ) is
  begin
    null;
  end;

end Ops_Pa.Transport_Pa.SendDataHandler_Pa;

