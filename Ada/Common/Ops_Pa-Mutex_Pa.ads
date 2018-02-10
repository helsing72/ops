--
-- Copyright (C) 2016-2018 Lennart Andersson.
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

with  Ada.Finalization;

package Ops_Pa.Mutex_Pa is

  protected type Mutex is

    entry Acquire;
    procedure Release;

  private
    Owned : Boolean := False;
  end Mutex;

  type Scope_Lock(Lock : access Mutex) is tagged limited private;

  -- Example usage:
  --   Mtx : aliased Mutex;
  --   ...
  --   procedure Op is
  --     S : Scope_Lock(Mtx'Access);   <-- will acquire Mtx at entry and release at exit from scope
  --   begin
  --     do operations;
  --   end;

private
  type Scope_Lock(Lock : access Mutex) is
    new Ada.Finalization.Limited_Controlled with null record;

  overriding procedure Initialize(Self : in out Scope_Lock);
  overriding procedure Finalize(Self : in out Scope_Lock);

end Ops_Pa.Mutex_Pa;

