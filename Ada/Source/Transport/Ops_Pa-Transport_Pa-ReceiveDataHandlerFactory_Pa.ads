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

with Ada.Containers.Indefinite_Ordered_Maps;

with Ops_Pa.OpsObject_Pa.Domain_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.Transport_Pa.ReceiveDataHandler_Pa,
     Ops_Pa.SerializableFactory_Pa.CompFactory_Pa,
     Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
     Ops_Pa.MemoryMap_Pa,
     Ops_Pa.Notifier_Pa,
     Com_Mutex_Pa;

use
     Ops_Pa.Transport_Pa.ReceiveDataHandler_Pa;

package Ops_Pa.Transport_Pa.ReceiveDataHandlerFactory_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type ReceiveDataHandlerFactory_Class    is new Ops_Class with private;
  type ReceiveDataHandlerFactory_Class_At is access all ReceiveDataHandlerFactory_Class'Class;

  -- Method prototype to call when we want to set UDP transport info for the participant info data
  type TOnUdpTransportInfoProc is new Integer; --///TODO procedure(ipaddress : string; port : Integer) of object;

  function Create( Proc : TOnUdpTransportInfoProc;
                   Reporter : Ops_Pa.Error_Pa.ErrorService_Class_At )
                  return ReceiveDataHandlerFactory_Class_At;

  function getReceiveDataHandler( Self : in out ReceiveDataHandlerFactory_Class;
                                  top : Ops_Pa.OpsObject_Pa.Topic_Pa.Topic_Class_At;
                                  dom : Ops_Pa.OpsObject_Pa.Domain_Pa.Domain_Class_At;
                                  opsObjectFactory : Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.SerializableInheritingTypeFactory_Class_At )
                                 return ReceiveDataHandler_Class_At;

  procedure releaseReceiveDataHandler( Self : in out ReceiveDataHandlerFactory_Class;
                                       top : Ops_Pa.OpsObject_Pa.Topic_Pa.Topic_Class_At );

private
-- ==========================================================================
--
-- ==========================================================================
  type HandlerInfo is
    record
      handler : ReceiveDataHandler_Class_At := null;
--      numUsers : Integer := 0;
    end record;

  function Less (Left, Right : String) return Boolean;
  function Equal (Left, Right : HandlerInfo) return Boolean;

  package MyMap is new Ada.Containers.Indefinite_Ordered_Maps(String, HandlerInfo, Less, Equal);

-- ==========================================================================
--
-- ==========================================================================
  type ReceiveDataHandlerFactory_Class is new Ops_Class with
    record
      -- Borrowed references
      OnUdpTransportInfoProc : TOnUdpTransportInfoProc;
      ErrorService : Ops_Pa.Error_Pa.ErrorService_Class_At := null;

      -- By Singelton, one ReceiveDataHandler per 'key' on this Participant
      ReceiveDataHandlerInstances : MyMap.Map;
      Lock : Com_Mutex_Pa.Mutex;
    end record;

  function makeKey( top : Ops_Pa.OpsObject_Pa.Topic_Pa.Topic_Class_At ) return String;


  procedure InitInstance( Self : in out ReceiveDataHandlerFactory_Class;
                          Proc : TOnUdpTransportInfoProc;
                          Reporter : Ops_Pa.Error_Pa.ErrorService_Class_At );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out ReceiveDataHandlerFactory_Class );


end Ops_Pa.Transport_Pa.ReceiveDataHandlerFactory_Pa;

