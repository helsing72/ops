--
-- Copyright (C) 2016-2019 Lennart Andersson.
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

package Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.OpsObjectFactory_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type OPSObjectFactory_Class    is new SerializableInheritingTypeFactory_Class with private;
  type OPSObjectFactory_Class_At is access all OPSObjectFactory_Class'Class;

  -- Return singelton instance of OPSObjectFactory.
  function getInstance return OPSObjectFactory_Class_At;


-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type OPSObjectFactoryImpl_Class    is new OPSObjectFactory_Class with private;
  type OPSObjectFactoryImpl_Class_At is access all OPSObjectFactoryImpl_Class'Class;

  -- Constructors
  function Create return OPSObjectFactoryImpl_Class_At;

-- ==========================================================================
-- Debug/Test helpers
-- ==========================================================================
  -- Removes all singleton instances. Note should only be used when looking for memory leaks.
  -- Requires that all OPS usage has been stopped and all objects freed.
  procedure Debug_TotalClear;

private
-- ==========================================================================
--
-- ==========================================================================
  type OPSObjectFactory_Class is new SerializableInheritingTypeFactory_Class with
    record
      null;
    end record;

  overriding procedure Finalize( Self : in out OPSObjectFactory_Class );

-- ==========================================================================
--
-- ==========================================================================
  type OPSObjectFactoryImpl_Class is new OPSObjectFactory_Class with
    record
      null;
    end record;

  procedure InitInstance( Self : in out OPSObjectFactoryImpl_Class );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out OPSObjectFactoryImpl_Class );

end Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.OpsObjectFactory_Pa;

