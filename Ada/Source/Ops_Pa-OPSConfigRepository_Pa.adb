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

with Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa.Domain_Pa;

use  Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa.Domain_Pa;

package body Ops_Pa.OPSConfigRepository_Pa is

  use type MyMap.cursor;

  function Less (Left, Right : String) return Boolean is
  begin
    return Left < Right;
  end;

  function Equal (Left, Right : OPSConfig_Class_At) return Boolean is
  begin
    return Left = Right;
  end;

  procedure InitInstance( Self : in out OPSConfigRepository_Class ) is
    cfg : ExtendedOPSConfig_Class_At := Create;
  begin
    Self.Config := OPSConfig_Class_At(cfg);
  end;

  function Create return OPSConfigRepository_Class_At is
    Self : OPSConfigRepository_Class_At := null;
  begin
    Self := new OPSConfigRepository_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  overriding procedure Finalize( Self : in out OPSConfigRepository_Class ) is

    procedure Process( Pos : in MyMap.Cursor ) is
      Value : OPSConfig_Class_At := MyMap.Element(Pos);
    begin
      if Value /= null then
        Free(Value);
      end if;
    end;

  begin
    -- Clear the internal list since it doesn't own the objects
    ExtendedOPSConfig_Class_At(Self.Config).Clear;
    Free(Self.Config);

    -- Need to free all objects in the dictionary
    Self.ConfigFiles.Iterate(Process'Access);
  end;


  -- Add domains from OPS configuration file "filename"
  -- if "domain" == '', all domains will be added otherwise only the specified "domain"
  -- Returns true if at least one domain added
  function Add( Self : in out OPSConfigRepository_Class; filename : string; domainID : string := "" ) return Boolean is
    S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.Mutex'Access);
  begin
    return Self.LocalAdd( filename, domainID );
  end;

  -- Must only be called while holding the Self.Mutex
  function LocalAdd( Self : in out OPSConfigRepository_Class; filename : string; domainID : string := "" ) return Boolean is
    config : OPSConfig_Class_At := null;
    domains : Domain_Class_At_Arr_At := null;
    pos : MyMap.Cursor;
    Result : Boolean := False;
  begin
    if domainID /= "" then
      -- Check if domain already exist
      if Self.LocalDomainExist( domainID ) then
        StaticErrorService.Report( "OPSConfigRepository", "Add", "domain '" & domainID & "' already exist" );
        return False;
      end if;
    end if;

    begin
      -- Check if file already read
      pos := Self.ConfigFiles.Find( filename );
      if pos /= MyMap.No_Element then
        -- Get the earlier read config
        config := MyMap.Element(pos);

      else
        -- Need to read file
        config := getConfig( filename );
        if config = null then
          StaticErrorService.Report( "TOPSConfigRepository", "Add", "Failed to parse file '" & filename & "'" );
          return False;
        end if;
        Self.ConfigFiles.Insert(filename, config);
      end if;
    exception
      when others =>
        StaticErrorService.Report( "TOPSConfigRepository", "Add", "Failed to parse file '" & filename & "'" );
        return False;
    end;

    -- Get all domains read from file
    domains := config.getDomains;

    -- Add the choosen one(s) to our list if not already there
    if domains /= null then
      for i in domains.all'Range loop
        if (domainID = "") or (domains(i).DomainID = domainID) then
          if Self.LocalDomainExist( domains(i).DomainID ) then
            StaticErrorService.Report( "TOPSConfigRepository", "Add", "domain '" & domains(i).DomainID & "' already exist" );
          else
            -- Add unique domains to our list
            ExtendedOPSConfig_Class_At(Self.Config).Add(domains(i));
            Result := True;
          end if;
        end if;
      end loop;
    end if;
    return Result;
  end;

  procedure Clear( Self : in out OPSConfigRepository_Class ) is
    S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.Mutex'Access);
  begin
    -- Since we just borrow the references to domains (they are owned by file cache)
    -- we can just clear the domain list in our OPSConfig object
    ExtendedOPSConfig_Class_At(Self.Config).Clear;
  end;

  function domainExist( Self : in out OPSConfigRepository_Class; domainID : string ) return Boolean is
    S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.Mutex'Access);
  begin
    return Self.LocalDomainExist( domainID );
  end;

  -- Must only be called while holding the Self.Mutex
  function LocalDomainExist( Self : in out OPSConfigRepository_Class; domainID : string ) return Boolean is
    domains : Domain_Class_At_Arr_At := null;
    Result : Boolean := False;
  begin
    domains := Self.Config.getDomains;
    if domains /= null then
      for i in domains.all'Range loop
        if domains(i).DomainID = domainID then
          Result := True;
        end if;
      end loop;
    end if;
    return Result;
  end;

  function getConfig( Self : in out OPSConfigRepository_Class; domainID : string := "" ) return OPSConfig_Class_At is
    domains : Domain_Class_At_Arr_At;
    S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.Mutex'Access);
  begin
    -- If no domain have been added, we try to add the default file
    -- This is for backward compatibility
    domains := Self.Config.getDomains;
    if domains = null or else domains.all'Length = 0 then
      if not Self.LocalAdd("ops_config.xml") then
        return null;
      end if;
    end if;

    if domainID /= "" then
      if not Self.LocalDomainExist( domainID ) then
        return null;
      end if;
    end if;

    return Self.Config;
  end;

  gInstance : OPSConfigRepository_Class_At := Create;

  function Instance return OPSConfigRepository_Class_At is
  begin
    return gInstance;
  end;

--///TODO  finalization
--    Free(gInstance);

end Ops_Pa.OPSConfigRepository_Pa;

