--
-- Copyright (C) 2017 Lennart Andersson.
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

with Ada.Containers.Indefinite_Ordered_Maps;

with Com_Mutex_Pa;

with Ops_Pa.OpsObject_Pa.OPSConfig_Pa;
use  Ops_Pa.OpsObject_Pa.OPSConfig_Pa;

package Ops_Pa.OPSConfigRepository_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type OPSConfigRepository_Class is new Ops_Class with private;
  type OPSConfigRepository_Class_At is access all OPSConfigRepository_Class'Class;

  -- Access to the singleton object
  function Instance return OPSConfigRepository_Class_At;

  -- ======================================================
  -- Add one or more domains from OPS configuration file "filename"
  -- if "domainID" == "", all domains will be added otherwise only the specified "domainID"
  -- Returns true if at least one domain was added
  function Add( Self : in out OPSConfigRepository_Class; filename : string; domainID : string := "" ) return Boolean;

  -- Remove all domain references from the repository (Note does not clear the file-cache)
  -- Note: Calling this while TParticipant, TPublisher or TSubscriber instances exist
  -- may have unwanted side effects
  procedure Clear( Self : in out OPSConfigRepository_Class );

  -- ======================================================
  -- Get a reference to the internal OPSConfig object
  -- if "domainID" <> "", the domain "domainID" must exist otherwise null is returned.
  function getConfig( Self : in out OPSConfigRepository_Class; domainID : string := "" ) return OPSConfig_Class_At;

  function domainExist( Self : in out OPSConfigRepository_Class; domainID : string ) return Boolean;

private
-- ==========================================================================
--
-- ==========================================================================
  function Less (Left, Right : String) return Boolean;
  function Equal (Left, Right : OPSConfig_Class_At) return Boolean;

  package MyMap is new Ada.Containers.Indefinite_Ordered_Maps(String, OPSConfig_Class_At, Less, Equal);

-- ==========================================================================
--
-- ==========================================================================
  type OPSConfigRepository_Class is new Ops_Class with
    record
      -- Our OPSConfig object containing references to all selectivly added domains
      Config : OPSConfig_Class_At := null;

      -- File cache with all added config files and their domains
      ConfigFiles : MyMap.Map;

      Mutex : aliased Com_Mutex_Pa.Mutex;
    end record;

  -- Must only be called while holding the Self.Mutex
  function LocalAdd( Self : in out OPSConfigRepository_Class; filename : string; domainID : string := "" ) return Boolean;
  function LocalDomainExist( Self : in out OPSConfigRepository_Class; domainID : string ) return Boolean;

  procedure InitInstance( Self : in out OPSConfigRepository_Class );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out OPSConfigRepository_Class );

end Ops_Pa.OPSConfigRepository_Pa;

