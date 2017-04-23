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

with Ada.Containers.Indefinite_Ordered_Maps;

with Com_Mutex_Pa;

with Ops_Pa.Error_Pa,
     Ops_Pa.SerializableFactory_Pa.CompFactory_Pa;
use  Ops_Pa.Error_Pa,
     Ops_Pa.SerializableFactory_Pa.CompFactory_Pa;

package body Ops_Pa.Participant_Pa is

  -- ===========================================================================

  function Less (Left, Right : String) return Boolean;
  function Equal (Left, Right : Participant_Class_At) return Boolean;

  package MyMap is new Ada.Containers.Indefinite_Ordered_Maps(String, Participant_Class_At, Less, Equal);

  use type MyMap.cursor;

  function Less (Left, Right : String) return Boolean is
  begin
    return Left < Right;
  end;

  function Equal (Left, Right : Participant_Class_At) return Boolean is
  begin
    return Left = Right;
  end;

  -- By Singelton, one Participant per 'domainId + participantID'
  gInstances : MyMap.Map;
  gMutex : Com_Mutex_Pa.Mutex;

  -- Get a Participant instance
  function getInstance(domainID : String) return Participant_Class_At is
  begin
    return getInstance(domainID, "DEFAULT_PARTICIPANT");
  end;

  function getInstance(domainID : String; participantID : String) return Participant_Class_At is
  begin
    return getInstance(domainID, participantID, "");
  end;

  function getInstance(domainID : String; participantID : String; configFile : String) return Participant_Class_At is
    key : String := domainID & "::" & participantID;
    result : Participant_Class_At := null;
    pos : MyMap.Cursor;
  begin
    gMutex.Acquire;
    begin
      pos := gInstances.Find( key );

      if pos = MyMap.No_Element then
        begin
          Result := Create(domainID, participantID, configFile);
          gInstances.Insert(key, Result);
        exception
          when others =>
            StaticErrorService.Report( "Participant", "Participant", "Unknown Exception" );
        end;
      else
        Result := MyMap.Element(pos);
      end if;
      gMutex.Release;
    exception
      when others =>
        gMutex.Release;
        raise;
    end;
    return Result;
  end;

  procedure releaseInstance( part : Participant_Class_At ) is
    key : String := part.domainID.all & "::" & part.participantID.all;
    pos : MyMap.Cursor;
  begin
    gMutex.Acquire;
    begin
      pos := gInstances.Find( key );

      if pos /= MyMap.No_Element then
        gInstances.Delete(pos);
      end if;

      Free(part);

      gMutex.Release;
    exception
      when others =>
        gMutex.Release;
        raise;
    end;
  end;

  -- ===========================================================================

  -- Constructors
  function Create( domainID : String; participantID : String; configFile : String ) return Participant_Class_At is
    Self : Participant_Class_At := null;
  begin
    Self := new Participant_Class;
    InitInstance( Self.all, domainID, participantID, configFile );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  procedure addTypeSupport( Self: in out Participant_Class; typeSupport : SerializableFactory_Class_At ) is
  begin
    Self.ObjectFactory.Add( typeSupport );
  end;

  function getSendDataHandler( Self: in out Participant_Class; top : Topic_Class_At) return SendDataHandler_Class_At is
    Result : SendDataHandler_Class_At := null;
  begin
    Result := Self.SendDataHandlerFactory.getSendDataHandler(top);

--TODO      if Result /= null then
--        FPartInfoDataMutex.Acquire;
--        begin
--        -- Need to add topic to partInfoData.publishTopics
--          FPartInfoData.addTopic(FPartInfoData.publishTopics, top);
--        finally
--          FPartInfoDataMutex.Release;
--        end;
--      end if;
    return Result;
  end;

  procedure releaseSendDataHandler( Self: in out Participant_Class; top : Topic_Class_At ) is
  begin
    Self.SendDataHandlerFactory.releaseSendDataHandler(top);

--TODO      FPartInfoDataMutex.Acquire;
--      try
--        -- Remove topic from partInfoData.publishTopics
--        FPartInfoData.removeTopic(FPartInfoData.publishTopics, top);
--      finally
--        FPartInfoDataMutex.Release;
--      end;
  end;

  -- Should only be used by Subscribers
  function getReceiveDataHandler( Self : in out Participant_Class; top : Topic_Class_At) return ReceiveDataHandler_Class_At is
    Result : ReceiveDataHandler_Class_At := null;
  begin
    Result := Self.ReceiveDataHandlerFactory.getReceiveDataHandler( top, Self.Domain, SerializableInheritingTypeFactory_Class_At(Self.ObjectFactory) );

    --///TODO partinfo stuff

    return Result;
  end;

  procedure releaseReceiveDataHandler( Self: in out Participant_Class; top : Topic_Class_At ) is
  begin
    Self.ReceiveDataHandlerFactory.releaseReceiveDataHandler( top );

    --///TODO partinfo stuff

  end;

  function getErrorService( Self : in out Participant_Class ) return ErrorService_Class_At is
  begin
    return Self.ErrorService;
  end;

  procedure ReportError( Self : in out Participant_Class; Error : Error_Class_At ) is
  begin
    Self.ErrorService.Report( Error );
  end;

  function getTopic( Self: Participant_Class; name : string) return Topic_Class_At is
    top : Topic_Class_At;
  begin
    top := Self.Domain.getTopic(name);
    if top /= null then
      top.SetParticipantID( Self.ParticipantID.all);
      top.SetDomainID( Self.DomainID.all );
    end if;
    return top;
  end;

  procedure InitInstance( Self : in out Participant_Class; domainID : String; participantID : String; configFile : String ) is
  begin
    Self.ParticipantID := Copy(participantID);
    Self.DomainID := Copy(domainID);
    --TODO FAliveTimeout := 1000;

    Self.ErrorService := Create;

    -- Read configFile
    if configFile = "" then
      -- This gets a reference to a singleton instance and should NOT be deleted.
      -- It may be shared between several Participants.
      Self.Config := getConfig;
    else
      -- This gets a reference to a unique instance and should eventually be deleted.
      -- Note however that the getDomain() call below returns a reference to an
      -- object internally in config.
      Self.Config := getConfig(configFile);
    end if;
    if Self.Config = null then
      StaticErrorService.Report( "Participant", "InitInstance", "getConfig() returned null" );
      raise EConfigException;
    end if;

    -- Get the domain from config. Note should not be deleted, owned by config.
    Self.Domain := Self.Config.getDomain(domainID);
    if Self.Domain = null then
      StaticErrorService.Report( "Participant", "InitInstance", "Domain '" & domainID & "' missing in config-file" );
      raise EConfigException;
    end if;

    -- Create a fatory instance for each participant
    Self.ObjectFactory := OPSObjectFactory_Class_At(Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.OpsObjectFactory_Pa.Create);

    -- Initialize static data in partInfoData
    --TODO

    --
    Self.SendDataHandlerFactory := Create(Self.Domain, Self.ErrorService);

    declare
      temp : TOnUdpTransportInfoProc := 0;
    begin
      Self.ReceiveDataHandlerFactory := Create(temp, Self.ErrorService);
    end;

    -- Partinfo topic / Listener
    --TODO

    -- Start our thread
    --TODO
  end;

  procedure Finalize( Self : in out Participant_Class ) is
  begin
    if Self.ReceiveDataHandlerFactory /= null then
      Free(Self.ReceiveDataHandlerFactory);
    end if;

    if Self.SendDataHandlerFactory /= null then
      Free(Self.SendDataHandlerFactory);
    end if;

    if Self.ObjectFactory /= null then
      Free(Self.ObjectFactory);
    end if;

    if Self.ErrorService /= null then
      Free(Self.ErrorService);
    end if;

    if Self.ParticipantID /= null then
      Dispose(Self.ParticipantID);
    end if;
    if Self.DomainID /= null then
      Dispose(Self.DomainID);
    end if;
  end;

end Ops_Pa.Participant_Pa;
