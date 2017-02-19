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

with Ada.Containers.Ordered_Maps;

with Com_Mutex_Pa;

with OPs_Pa.Error_Pa;
use  Ops_Pa.Error_Pa;

package body Ops_Pa.Participant_Pa is

  -- ===========================================================================

  function Less (Left, Right : String_At) return Boolean;
  function Equal (Left, Right : Participant_Class_At) return Boolean;

  package MyMap is new Ada.Containers.Ordered_Maps(String_At, Participant_Class_At, Less, Equal);

  use type MyMap.cursor;

  function Less (Left, Right : String_At) return Boolean is
  begin
    return Left.all < Right.all;
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
    key : String_At := new String'(domainID & "::" & participantID);
    result : Participant_Class_At := null;
    pos : MyMap.Cursor;
    error : BasicError_Class_At := null;
  begin
    gMutex.Acquire;
    begin
      pos := gInstances.Find( key );

      if pos = MyMap.No_Element then
        begin
          Result := Create(domainID, participantID, configFile);
          gInstances.Insert(key, Result);
          key := null;
        exception
          when others =>
            error := Create("Participant", "Participant", "Unknown Exception");
            StaticErrorService.Report( Error_Class_At(error) );
        end;
      else
        Result := MyMap.Element(pos);
      end if;
    exception
      when others =>
        gMutex.Release;
        if key /= null then
          Dispose(key);
        end if;
        raise;
    end;
    gMutex.Release;
    if key /= null then
      Dispose(key);
    end if;
    return Result;
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




  procedure InitInstance( Self : in out Participant_Class; domainID : String; participantID : String; configFile : String ) is
  begin
    Self.ParticipantID := Copy(participantID);
    Self.DomainID := Copy(domainID);
    --TODO FAliveTimeout := 1000;

    Self.ErrorService := Create;

    -- Read configFile
    --TODO

    -- Get the domain from config. Note should not be deleted, owned by config.
    --TODO Self.Domain := FConfig.getDomain(domainID);
    --if Self.Domain = null then
    --  raise ECommException.Create('Domain "' + domainID + '" missing in config-file');
    --end if;
    Self.Domain := Create;


    Self.SendDataHandlerFactory := Create(Self.Domain, Self.ErrorService);

  end;

  procedure Finalize( Self : in out Participant_Class ) is
  begin
    if Self.SendDataHandlerFactory /= null then
      Free(Self.SendDataHandlerFactory);
    end if;

    -- TODO inte tas bort då config finns då den äger domain
    if Self.Domain /= null then
      Free(Self.Domain);
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
