--
-- Copyright (C) 2017-2018 Lennart Andersson.
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

with Ops_Pa.OpsObject_Pa.ParticipantInfoData_Pa,
     Ops_Pa.OpsObject_Pa.TopicInfoData_Pa,
     Ops_Pa.Transport_Pa.SendDataHandler_Pa.McUdp_Pa;

use  Ops_Pa.OpsObject_Pa.ParticipantInfoData_Pa,
     Ops_Pa.OpsObject_Pa.TopicInfoData_Pa,
     Ops_Pa.Transport_Pa.SendDataHandler_Pa.McUdp_Pa;

package body Ops_Pa.ParticipantInfoDataListener_Pa is

  function Create(dom : Domain_Class_At; partInfoTopic : Topic_Class_At; Reporter : ErrorService_Class_At) return ParticipantInfoDataListener_Class_At is
    Self : ParticipantInfoDataListener_Class_At := null;
  begin
    Self := new ParticipantInfoDataListener_Class;
    InitInstance( Self.all, Self, dom, partInfoTopic, Reporter );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out ParticipantInfoDataListener_Class;
                          SelfAt : ParticipantInfoDataListener_Class_At;
                          dom : Domain_Class_At;
                          partInfoTopic : Topic_Class_At;
                          Reporter : ErrorService_Class_At ) is
  begin
    Self.SelfAt := SelfAt;
    Self.Domain := dom;
    Self.PartInfoTopic := partInfoTopic;
    Self.ErrorService := Reporter;
  end;

  overriding procedure Finalize( Self : in out ParticipantInfoDataListener_Class ) is
  begin
    Self.removeSubscriber;       -- Just to be sure
  end;

  procedure connectUdp( Self : in out ParticipantInfoDataListener_Class; top : Topic_Class_At; handler : SendDataHandler_Class_At) is
    S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.Mutex'Access);
  begin
    if Self.PartInfoSub = null then
      if not Self.setupSubscriber then
        -- Generate an error message if we come here with domain->getMetaDataMcPort() == 0,
        -- it means that we have UDP topics that require meta data but user has disabled it.
        if Self.ErrorService /= null then
          Self.ErrorService.Report(
            "ParticipantInfoDataListener", "connectUdp",
            "UDP topic '" & top.Name & "' won't work since Meta Data disabled in config-file");
        end if;
        return;
      end if;
    end if;

    -- Since we only have one common UDP SendDataHandler, its enough to count connected topics
    Self.NumUdpTopics := Self.NumUdpTopics + 1;

    Self.SendDataHandler := handler;
  end;

  procedure disconnectUdp( Self : in out ParticipantInfoDataListener_Class; top : Topic_Class_At; handler : SendDataHandler_Class_At) is
    S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.Mutex'Access);
  begin
    -- Remove topic from list so we know if the subscriber is needed
    Self.NumUdpTopics := Self.NumUdpTopics - 1;

    if Self.NumUdpTopics = 0 then
      Self.SendDataHandler := null;

--TODO  if num tcp topics = 0 then begin
        Self.removeSubscriber;
--    end;
    end if;
  end;

--  procedure TParticipantInfoDataListener.connectTcp(top: TTopic;
--    handler: TObject);
--  begin
--    FMutex.Acquire;
--    try
--      if not Assigned(FPartInfoSub) then
--        if not setupSubscriber then
--          -- Generate an error message if we come here with domain->getMetaDataMcPort() == 0,
--          -- it means that we have TCP topics that require meta data but user has disabled it.
--          if Assigned(FErrorService) then
--            FErrorService.Report(TBasicError.Create(
--              'ParticipantInfoDataListener', 'connectTcp',
--              'TCP topic "' + string(top.Name) + '" won''t work since Meta Data disabled in config-file'));
--          end if;
--          Exit;
--        end if;
--      end if;
--
--      --TODO add to map
--
--    finally
--      FMutex.Release;
--    end;
--  end;

--  procedure TParticipantInfoDataListener.disconnectTcp(top: TTopic;
--    handler: TObject);
--  begin
--    FMutex.Acquire;
--    try
--      --TODO remove from map
--    finally
--      FMutex.Release;
--    end;
--  end;

  function setupSubscriber( Self : in out ParticipantInfoDataListener_Class) return Boolean is
  begin
    -- Check that user hasn't disabled the meta data
    if Self.Domain.MetaDataMcPort = 0 then
      return False;
    end if;

    Self.PartInfoSub := Create(Self.PartInfoTopic);
    Self.PartInfoSub.AddListener(Ops_Pa.Subscriber_Pa.MessageNotifier_Pa.Listener_Interface_At(Self.SelfAt));
    Self.PartInfoSub.Start;

    return True;
  end;

  procedure removeSubscriber( Self : in out ParticipantInfoDataListener_Class ) is
  begin
    if Self.PartInfoSub /= null then
      Self.PartInfoSub.Stop;
      Free(Self.PartInfoSub);
      Self.PartInfoSub := null;
    end if;
  end;

  procedure OnNotify( Self : in out ParticipantInfoDataListener_Class; Sender : in Ops_Class_At; Item : in OPSMessage_Class_At ) is
    partInfo : ParticipantInfoData_Class_At := null;
  begin
    if Item /= null then
      if Item.Data.all in ParticipantInfoData_Class'Class then
        partInfo := ParticipantInfoData_Class_At(Item.Data);
        -- Is it on our domain?
        if partInfo.Domain.all = Self.Domain.DomainID then
          declare
            S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.Mutex'Access);
          begin
            -- Checks for UDP transport
            if Self.SendDataHandler /= null and partInfo.subscribeTopics /= null then
              for i in partInfo.subscribeTopics'Range loop
                if partInfo.subscribeTopics(i).transport.all = TRANSPORT_UDP
                  --TODO participant->hasPublisherOn(partInfo->subscribeTopics[i].name) )
                then
                  -- Do an add sink here
                  McUdpSendDataHandler_Class_At(Self.SendDataHandler).addSink(
                     partInfo.subscribeTopics(i).name.all, partInfo.ip.all, partInfo.mc_udp_port);
                end if;
              end loop;
            end if;

            -- Checks for TCP transport
            --  //          for (unsigned int i = 0; i < partInfo->publishTopics.size(); i++)
            --  //          {
            --  //						if (partInfo->publishTopics[i].transport == Topic::TRANSPORT_TCP) {
            --  //							--/TODO lookup topic in map. If found call handler
            --  //            }
            --  //          }
          end;
        end if;
      else
        if Self.ErrorService /= null then
          Self.ErrorService.Report("ParticipantInfoDataListener", "onNewData", "Data could not be cast as expected.");
        end if;
      end if;
    end if;
  end;

end Ops_Pa.ParticipantInfoDataListener_Pa;

