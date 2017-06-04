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

with Com_Mutex_Pa;

with Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.OpsObject_Pa.Domain_Pa,
     Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
     Ops_Pa.Subscriber_Pa,
     Ops_Pa.Transport_Pa.SendDataHandler_Pa;

use  Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.OpsObject_Pa.Domain_Pa,
     Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
     Ops_Pa.Subscriber_Pa,
     Ops_Pa.Transport_Pa.SendDataHandler_Pa;

package Ops_Pa.ParticipantInfoDataListener_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type ParticipantInfoDataListener_Class    is new Ops_Class and
    Ops_Pa.Subscriber_Pa.MessageNotifier_Pa.Listener_Interface with private;
  type ParticipantInfoDataListener_Class_At is access all ParticipantInfoDataListener_Class'Class;

  function Create(dom : Domain_Class_At; partInfoTopic : Topic_Class_At; Reporter : ErrorService_Class_At) return ParticipantInfoDataListener_Class_At;

  procedure connectUdp( Self : in out ParticipantInfoDataListener_Class; top : Topic_Class_At; handler : SendDataHandler_Class_At);
  procedure disconnectUdp( Self : in out ParticipantInfoDataListener_Class; top : Topic_Class_At; handler : SendDataHandler_Class_At);

--    procedure connectTcp( Self : in out ParticipantInfoDataListener_Class; top : Topic_Class_At; handler : TObject);
--    procedure disconnectTcp( Self : in out ParticipantInfoDataListener_Class; top : Topic_Class_At; handler : TObject);

private
-- ==========================================================================
--
-- ==========================================================================
  type ParticipantInfoDataListener_Class is new Ops_Class and
    Ops_Pa.Subscriber_Pa.MessageNotifier_Pa.Listener_Interface with
     record
       -- Borrowed references
       Domain : Domain_Class_At := null;
       PartInfoTopic : Topic_Class_At := null;
       ErrorService : ErrorService_Class_At := null;
       SendDataHandler : SendDataHandler_Class_At := null;

       -- Owned references
       SelfAt : ParticipantInfoDataListener_Class_At := null;
       Mutex : aliased Com_Mutex_Pa.Mutex;
       PartInfoSub : Subscriber_Class_At := null;

       NumUdpTopics : Integer := 0;
     end record;

  function setupSubscriber( Self : in out ParticipantInfoDataListener_Class) return Boolean;
  procedure removeSubscriber( Self : in out ParticipantInfoDataListener_Class );

  procedure OnNotify( Self : in out ParticipantInfoDataListener_Class; Sender : in Ops_Class_At; Item : in OPSMessage_Class_At );

  procedure InitInstance( Self : in out ParticipantInfoDataListener_Class;
                          SelfAt : ParticipantInfoDataListener_Class_At;
                          dom : Domain_Class_At;
                          partInfoTopic : Topic_Class_At;
                          Reporter : ErrorService_Class_At );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out ParticipantInfoDataListener_Class );

end Ops_Pa.ParticipantInfoDataListener_Pa;

