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

package body Ops_Pa.OpsObject_Pa.OPSConfig_Pa is

  -- Constructors
  function Create return OPSConfig_Class_At is
    Self : OPSConfig_Class_At := null;
  begin
    Self := new OPSConfig_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  -- Helpers for handling [de]serializing of fixed arrays
  procedure Domain_Class_InoutDynArr is new inoutdynarr2(Domain_Class, Domain_Class_At, Domain_Class_At_Arr, Domain_Class_At_Arr_At);

  procedure Serialize( Self : in out OPSConfig_Class; archiver : ArchiverInOut_Class_At) is
  begin
    Serialize( OpsObject_Class(Self), archiver );
    Domain_Class_InoutDynArr(archiver, "domains", Self.domains);
  end;

  -- Returns a newely allocated deep copy/clone of Self.
  function Clone( Self : OPSConfig_Class ) return OpsObject_Class_At is
    Result : OPSConfig_Class_At := null;
  begin
    Result := Create;
    Self.FillClone( OpsObject_Class_At(Result) );
    return OpsObject_Class_At(Result);
  end Clone;

  -- Helpers
  procedure Dispose is new Ada.Unchecked_Deallocation( Domain_Class_Arr, Domain_Class_Arr_At );
  procedure Dispose is new Ada.Unchecked_Deallocation( Domain_Class_At_Arr, Domain_Class_At_Arr_At );

  procedure Clear( Arr : in out Domain_Class_At_Arr) is
  begin
    for i in Arr'Range loop
      if Arr(i) /= null then
        Free(Arr(i));
      end if;
    end loop;
  end;

  -- Fills the parameter obj with all values from Self.
  procedure FillClone( Self : OPSConfig_Class; obj : OpsObject_Class_At ) is
  begin
    FillClone( OpsObject_Class(Self), obj );
    if obj.all in OPSConfig_Class'Class then
      if OPSConfig_Class(obj.all).domains /= null then
        Clear(OPSConfig_Class(obj.all).domains.all);
        Dispose(OPSConfig_Class(obj.all).domains);
      end if;
      if Self.domains /= null then
        OPSConfig_Class(obj.all).domains := new Domain_Class_At_Arr(Self.domains'Range);
        for i in Self.domains'Range loop
          if Self.domains(i) /= null then
            OPSConfig_Class(obj.all).domains(i) := Domain_Class_At(Clone(Self.domains(i).all));
          end if;
        end loop;
      end if;
    end if;
  end;

  procedure InitInstance( Self : in out OPSConfig_Class ) is
  begin
    InitInstance( OpsObject_Class(Self) );
  end;

  procedure Finalize( Self : in out OPSConfig_Class ) is
  begin
    if Self.domains /= null then
      Clear(Self.domains.all);
      Dispose(Self.domains);
    end if;

    Finalize( OpsObject_Class(Self) );
  end;

  -- Returns a reference to the given Domain
  -- NOTE: The OPSConfig still owns it
  function getDomain( Self : OPSConfig_Class; domainID : string) return Domain_Class_At is
  begin
    for i in Self.domains.all'Range loop
      if Self.domains(i).all.DomainID = domainID then
        return Self.domains(i);
      end if;
    end loop;
    return null;
  end;

  -- Returns references to the internal Domains
  -- NOTE: The OPSConfig still owns them
  function getDomains( Self : OPSConfig_Class ) return Domain_Class_At_Arr_At is
  begin
    return Self.Domains;
  end;

-- ---------------------------------------------------------------------------

  function Create return DefaultOPSConfigImpl_Class_At is
    Self : DefaultOPSConfigImpl_Class_At := null;
  begin
    Self := new DefaultOPSConfigImpl_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure Serialize( Self : in out DefaultOPSConfigImpl_Class; archiver : ArchiverInOut_Class_At) is
  begin
    Serialize( OPSConfig_Class(Self), archiver );
  end;

  procedure InitInstance( Self : in out DefaultOPSConfigImpl_Class ) is
  begin
    InitInstance( OPSConfig_Class(Self) );
    AppendType( OPSConfig_Class(Self), "DefaultOPSConfigImpl" );
  end;

  procedure Finalize( Self : in out DefaultOPSConfigImpl_Class ) is
  begin
    Finalize( OPSConfig_Class(Self) );
  end;

-- ---------------------------------------------------------------------------


  gConfiguration : OPSConfig_Class_At := null;
--  gMutex : TMutex;


  -- Returns a reference to a unique instance and should be deleted.
  -- See also releaseConfig below
  function getConfig(configFile : String) return OPSConfig_Class_At is
--    archiver : TXMLArchiverIn;
--    list : TStringList;
  begin
    raise Not_Yet_Implemented;
    return null;
--      Result := nil;
--
--      list := TStringList.Create;
--      try
--        list.LoadFromFile(configFile);
--      archiver := nil;
--      try
--        archiver := TXMLArchiverIn.Create(list.Text, 'root', TOPSObjectFactory.getInstance);
--      Result := TOPSConfig(archiver.inout2('ops_config', TSerializable(Result)));
--      finally
--        FreeAndNil(archiver);
--    end;
--    finally
--      FreeAndNil(list);
--  end;
  end;

  -- Returns a reference to a singleton instance and should NOT be deleted.
  -- See also releaseConfig below
  function getConfig return OPSConfig_Class_At is
  begin
    raise Not_Yet_Implemented;
    return null;
--    gMutex.Acquire;
--    try
--    	if not Assigned(gConfiguration) then begin
--        gConfiguration := TOPSConfigRepository.Instance.getConfig();
--      end;
--    finally
--      gMutex.Release;
--    end;
--    Result := gConfiguration;
  end;

  -- Help method that deletes the config if it isn't the singleton instance
  -- The variable used in the call will always be set to null
  procedure releaseConfig(cfg : in out OPSConfig_Class_At) is
  begin
    -- We don't want to delete the singleton instance
    if cfg /= gConfiguration and cfg /= null then
      Free(cfg);
    end if;
    cfg := null;
  end;


begin
  null;
--  gMutex := TMutex.Create;

--  finalization
--    //Don't delete config (gConfiguration) since we got it from the TOPSConfigRepository who will delete it
--    //FreeAndNil(gConfiguration);
--    FreeAndNil(gMutex);
end Ops_Pa.OpsObject_Pa.OPSConfig_Pa;


