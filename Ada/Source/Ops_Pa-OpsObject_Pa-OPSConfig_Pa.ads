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

with Ops_Pa.OpsObject_Pa.Domain_Pa,
     Ops_Pa.ArchiverInOut_Pa;
use  Ops_Pa.OpsObject_Pa.Domain_Pa,
     Ops_Pa.ArchiverInOut_Pa;

package Ops_Pa.OpsObject_Pa.OPSConfig_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type OPSConfig_Class is new OpsObject_Class with private;
  type OPSConfig_Class_At is access all OPSConfig_Class'Class;

  -- Help types used in case other idl's has vector's of the class
  type OPSConfig_Class_Arr is array(Integer range <>) of aliased OPSConfig_Class;
  type OPSConfig_Class_Arr_At is access all OPSConfig_Class_Arr;
  type OPSConfig_Class_At_Arr is array(Integer range <>) of OPSConfig_Class_At;
  type OPSConfig_Class_At_Arr_At is access all OPSConfig_Class_At_Arr;

  -- Constructors
  function Create return OPSConfig_Class_At;

  overriding procedure Serialize( Self : in out OPSConfig_Class; archiver : ArchiverInOut_Class_At);

  -- Returns a newely allocated deep copy/clone of Self.
  overriding function Clone( Self : OPSConfig_Class ) return OpsObject_Class_At;

  -- Fills the parameter obj with all values from Self.
  overriding procedure FillClone( Self : OPSConfig_Class; obj : OpsObject_Class_At );

  -- Returns a reference to a singleton instance and should NOT be deleted.
  -- See also releaseConfig below
  function getConfig return OPSConfig_Class_At;

  -- Returns a reference to a unique instance and should be deleted.
  -- See also releaseConfig below
  function getConfig(configFile : String) return OPSConfig_Class_At;

  -- Help method that deletes the config if it isn't the singleton instance
  -- The variable used in the call will always be set to null
  procedure releaseConfig(cfg : in out OPSConfig_Class_At);

  -- Returns a reference to the given Domain
  -- NOTE: The OPSConfig still owns it
  function getDomain( Self : OPSConfig_Class; domainID : string) return Domain_Class_At;

  -- Returns references to the internal Domains
  -- NOTE: The OPSConfig still owns them
  function getDomains( Self : OPSConfig_Class ) return Domain_Class_At_Arr_At;


-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type DefaultOPSConfigImpl_Class is new OPSConfig_Class with private;
  type DefaultOPSConfigImpl_Class_At is access all DefaultOPSConfigImpl_Class'Class;

  function Create return DefaultOPSConfigImpl_Class_At;

  overriding procedure Serialize( Self : in out DefaultOPSConfigImpl_Class; archiver : ArchiverInOut_Class_At);

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type ExtendedOPSConfig_Class is new DefaultOPSConfigImpl_Class with private;
  type ExtendedOPSConfig_Class_At is access all ExtendedOPSConfig_Class'Class;

  function Create return ExtendedOPSConfig_Class_At;

  procedure Add( Self : in out ExtendedOPSConfig_Class; domain : Domain_Class_At);
  procedure Remove( Self : in out ExtendedOPSConfig_Class; domain : Domain_Class_At);
  procedure Clear( Self : in out ExtendedOPSConfig_Class );

private
-- ==========================================================================
--
-- ==========================================================================
  type OPSConfig_Class is new OpsObject_Class with
    record
      domains : Domain_Class_At_Arr_At := null;
    end record;

  procedure InitInstance( Self : in out OPSConfig_Class );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out OPSConfig_Class );

-- ==========================================================================
--
-- ==========================================================================
  type DefaultOPSConfigImpl_Class is new OPSConfig_Class with null record;

  procedure InitInstance( Self : in out DefaultOPSConfigImpl_Class );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out DefaultOPSConfigImpl_Class );

-- ==========================================================================
--
-- ==========================================================================
  type ExtendedOPSConfig_Class is new DefaultOPSConfigImpl_Class with null record;

  procedure InitInstance( Self : in out ExtendedOPSConfig_Class );

  overriding procedure Finalize( Self : in out ExtendedOPSConfig_Class );

end Ops_Pa.OpsObject_Pa.OPSConfig_Pa;

