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

with Interfaces; use Interfaces;

package Ops_Pa.SyncPrimitives_Pa is
  
  --------------------------------------------------------------------------
  -- Increments the variable in a task safe manner.
  -- Returns:
  --     > 0  Value after increment is > 0
  --     = 0  Value after increment is = 0
  --     < 0  Value after increment is < 0
  --------------------------------------------------------------------------
  function InterlockedIncrement (Target : access Integer_32) return Integer_32;
  pragma Import(Stdcall, InterlockedIncrement, "InterlockedIncrement");
  
  --------------------------------------------------------------------------
  -- Decrements the variable in a task safe manner.
  -- Returns:
  --     > 0  Value after decrement is > 0
  --     = 0  Value after decrement is = 0
  --     < 0  Value after decrement is < 0
  --------------------------------------------------------------------------
  function InterlockedDecrement (Target : access Integer_32) return Integer_32;
  pragma Import(Stdcall, InterlockedDecrement, "InterlockedDecrement");
  
  --------------------------------------------------------------------------
  -- Exchanges a pair of 32 bit values in a task safe manner.
  -- Returns the old value
  --------------------------------------------------------------------------
  function InterlockedExchange (Target   : access Unsigned_32;
                                NewValue : Unsigned_32) return Unsigned_32;
  pragma Import(Stdcall, InterlockedExchange, "InterlockedExchange");
  
end Ops_Pa.SyncPrimitives_Pa;
 
