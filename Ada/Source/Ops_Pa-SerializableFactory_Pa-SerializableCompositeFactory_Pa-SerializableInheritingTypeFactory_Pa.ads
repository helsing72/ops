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

package Ops_Pa.SerializableFactory_Pa.SerializableCompositeFactory_Pa.SerializableInheritingTypeFactory_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type SerializableInheritingTypeFactory_Class    is new SerializableCompositeFactory_Class with null record;
  type SerializableInheritingTypeFactory_Class_At is access all SerializableInheritingTypeFactory_Class'Class;

  -- Constructors
  function Create return SerializableInheritingTypeFactory_Class_At;

  -- Tries to construct the most specialized object in the given typeString list
  overriding function Make( Self : SerializableInheritingTypeFactory_Class; types : string) return Serializable_Class_At;

private
-- ==========================================================================
--
-- ==========================================================================

  procedure InitInstance( Self : in out SerializableInheritingTypeFactory_Class );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out SerializableInheritingTypeFactory_Class );

end Ops_Pa.SerializableFactory_Pa.SerializableCompositeFactory_Pa.SerializableInheritingTypeFactory_Pa;
