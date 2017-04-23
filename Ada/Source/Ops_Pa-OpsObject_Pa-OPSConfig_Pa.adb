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

with Ada.Text_IO;
with Ada.IO_Exceptions;

with Com_Mutex_Pa;

with Ops_Pa.Error_Pa,
     Ops_Pa.ArchiverInOut_Pa.XMLArchiverIn_Pa,
     Ops_Pa.SerializableFactory_Pa.CompFactory_Pa,
     Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.OpsObjectFactory_Pa,
     Ops_Pa.OPSConfigRepository_Pa;

use  Ops_Pa.Error_Pa,
     Ops_Pa.ArchiverInOut_Pa.XMLArchiverIn_Pa,
     Ops_Pa.SerializableFactory_Pa.CompFactory_Pa,
     Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.OpsObjectFactory_Pa;

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

  function Create return ExtendedOPSConfig_Class_At is
    Self : ExtendedOPSConfig_Class_At := null;
  begin
    Self := new ExtendedOPSConfig_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  procedure InitInstance( Self : in out ExtendedOPSConfig_Class ) is
  begin
    InitInstance( DefaultOPSConfigImpl_Class(Self) );
  end;

  procedure Finalize( Self : in out ExtendedOPSConfig_Class ) is
  begin
    Finalize( DefaultOPSConfigImpl_Class(Self) );
  end;

  procedure Add( Self : in out ExtendedOPSConfig_Class; domain : Domain_Class_At) is
    old : Domain_Class_At_Arr_At := Self.domains;
  begin
    if Self.Domains = null then
      Self.Domains := new Domain_Class_At_Arr(0..0);
    else
      Self.Domains := new Domain_Class_At_Arr(old'First..old'Last+1);
      Self.Domains(old'Range) := old.all;
      Dispose(old);
    end if;
    Self.Domains(Self.Domains'Last) := domain;
  end;

  procedure Remove( Self : in out ExtendedOPSConfig_Class; domain : Domain_Class_At) is
    Idx : Integer := -1;
    Found : Boolean := False;
  begin
    if Self.domains /= null then
      for i in Self.domains'Range loop
        if Self.domains(i) = domain then
          Idx := i;
          Found := True;
          exit;
        end if;
      end loop;
      if Found then
        if Self.domains'Length = 1 then
          Dispose(Self.domains);
          Self.domains := null;
        else
          if Idx < Self.domains'Last then
            for i in Idx+1 .. Self.domains'Last loop
              Self.domains(i-1) := Self.domains(i);
            end loop;
          end if;
          declare
            old : Domain_Class_At_Arr_At := Self.domains;
          begin
            Self.Domains := new Domain_Class_At_Arr(old'First..old'Last-1);
            Self.Domains(Self.domains'Range) := old.all(old'First..old'Last-1);
            Dispose(old);
          end;
        end if;
      end if;
    end if;
  end;

  procedure Clear( Self : in out ExtendedOPSConfig_Class ) is
  begin
    -- We don't own the domains, so just clear array
    if Self.domains /= null then
      Dispose(Self.domains);
      Self.domains := null;
    end if;
  end;

-- ---------------------------------------------------------------------------

  gConfiguration : OPSConfig_Class_At := null;
  gMutex : Com_Mutex_Pa.Mutex;


  function ReadFile(Name : String) return String_At is
    Result : String_At := new String'("");
    File : Ada.Text_IO.File_Type;

    procedure Append(str : String) is
      Old : String_At := Result;
    begin
      Result := new String'(Old.all & str);
      Dispose(old);
    end;

  begin
    -- Open file
    Ada.Text_IO.Open(File, Ada.Text_IO.In_File, Name);
    begin
      -- Read complete file
      while not Ada.Text_IO.End_Of_File(File) loop
        Append(Ada.Text_IO.Get_Line(File));
      end loop;
      -- Close file
      Ada.Text_IO.Close(File);
      return Result;
    exception
      when others =>
        Ada.Text_IO.Close(File);
        raise;
    end;
  exception
    when others =>
      Dispose(Result);
      raise;
  end;

  -- Returns a reference to a unique instance and should be deleted.
  -- See also releaseConfig below
  function getConfig(configFile : String) return OPSConfig_Class_At is
    archiver : XMLArchiverIn_Class_At := null;
    Result : OPSConfig_Class_At := null;
    list : String_At := null;
  begin
    begin
      list := ReadFile(configFile);
      archiver := XMLArchiverIn_Pa.Create(list.all, "root", SerializableInheritingTypeFactory_Class_At(getInstance));
      begin
        Result := OPSConfig_Class_At(archiver.inout2("ops_config", Serializable_Class_At(Result)));
      exception
        when others =>
          StaticErrorService.Report( "OPSConfig", "getConfig", "Unknown exception during XML parsing" );
      end;
      Free(archiver);
    exception
      when Ada.IO_Exceptions.Name_Error =>
        StaticErrorService.Report( "OPSConfig", "getConfig", "File '" & configFile & "' not found" );
      when others =>
        StaticErrorService.Report( "OPSConfig", "getConfig", "Unknown exception during file reading" );
    end;
    if list /= null then
      Dispose(list);
    end if;
    return Result;
  end;

  -- Returns a reference to a singleton instance and should NOT be deleted.
  -- See also releaseConfig below
  function getConfig return OPSConfig_Class_At is
  begin
    gMutex.Acquire;
    begin
      if gConfiguration = null then
        gConfiguration := Ops_Pa.OPSConfigRepository_Pa.Instance.getConfig;
      end if;
      gMutex.Release;
    exception
      when others =>
        gMutex.Release;
        raise;
    end;
    return gConfiguration;
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


--  finalization
--    //Don't delete config (gConfiguration) since we got it from the TOPSConfigRepository who will delete it
--    //FreeAndNil(gConfiguration);

end Ops_Pa.OpsObject_Pa.OPSConfig_Pa;


