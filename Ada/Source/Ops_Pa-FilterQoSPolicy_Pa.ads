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

package Ops_Pa.FilterQoSPolicy_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type FilterQoSPolicy_Class    is abstract new Ops_Class with null record;
  type FilterQoSPolicy_Class_At is access all FilterQoSPolicy_Class'Class;

  -- Applies a filter in the receiving process in Subscribers.
  -- Returning false from a filter indicates that this data sample (OPSObject)
  -- shall not be propagated to the application layer.
  function ApplyFilter( Self : FilterQoSPolicy_Class; o : OpsObject_Class_At) return Boolean is abstract;

end Ops_Pa.FilterQoSPolicy_Pa;

