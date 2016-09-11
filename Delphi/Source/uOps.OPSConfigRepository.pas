unit uOps.OPSConfigRepository;

(**
*
* Copyright (C) 2016 Lennart Andersson.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*)

interface

uses System.Generics.Collections,
     System.SyncObjs,
     uOps.OPSConfig;

type
  TOPSConfigRepository = class(TObject)
  private
    // Our OPSConfig object containing references to all selectivly added domains
    FConfig : TOPSConfig;

    // File cache with all added config files and their domains
    FConfigFiles : TDictionary<string, TOPSConfig>;

    FMutex : TMutex;

    constructor Create;

  public
    destructor Destroy; override;

    // Access to the singleton object
    class function Instance : TOPSConfigRepository;

    // ======================================================
    // Add one or more domains from OPS configuration file "filename"
    // if "domainID" == '', all domains will be added otherwise only the specified "domainID"
    // Returns true if at least one domain was added
    function Add( filename : string; domainID : string = '' ) : Boolean;

    // Remove all domain references from the repository (Note does not clear the file-cache)
    // Note: Calling this while TParticipant, TPublisher or TSubscriber instances exist
    // may have unwanted side effects
    procedure Clear;

    // ======================================================
    // Get a reference to the internal OPSConfig object
    // if "domainID" <> '', the domain "domainID" must exist otherwise NIL is returned.
    function getConfig( domainID : string = '' ) : TOPSConfig;

    function domainExist( domainID : string ) : Boolean;
  end;

implementation

uses SysUtils,
     uOps.Error,
     uOps.Domain;

type
  TExtendedOPSConfig = class(TDefaultOPSConfigImpl)
  public
    procedure Add(domain : TDomain);
    procedure Remove(domain : TDomain);
    procedure Clear;
  end;

var
  gInstance : TOPSConfigRepository;


{ TExtendedOPSConfig }

procedure TExtendedOPSConfig.Add(domain: TDomain);
var
  Len : Integer;
begin
  Len := Length(FDomains);
  SetLength(FDomains, Len + 1);
  FDomains[Len] := domain;
end;

procedure TExtendedOPSConfig.Clear;
begin
  SetLength(FDomains, 0);
end;

procedure TExtendedOPSConfig.Remove(domain: TDomain);
var
  Len, i, Found : Integer;
begin
  Found := -1;
  Len := Length(FDomains);
  for i := 0 to Len - 1 do begin
    if FDomains[i] = domain then begin
      Found := i;
      Break;
    end;
  end;
  if Found >= 0 then begin
    for i := Found+1 to Len - 1 do begin
      FDomains[i-1] := FDomains[i];
    end;
    SetLength(FDomains, Len - 1);
  end;
end;

{ TOPSConfigRepository }

constructor TOPSConfigRepository.Create;
begin
  inherited;
  FMutex := TMutex.Create;
  FConfigFiles := TDictionary<string, TOPSConfig>.Create;
  FConfig := TExtendedOPSConfig.Create;
end;

destructor TOPSConfigRepository.Destroy;
var
  Value : TOPSConfig;
begin
  // Clear the internal list since it doesn't own the objects
  (FConfig as TExtendedOPSConfig).Clear;
  FreeAndNil(FConfig);

  // Need to free all objects in the dictionary
  for Value in FConfigFiles.Values do begin
    if Assigned(Value) then Value.Free;
  end;
  FreeAndNil(FConfigFiles);

  FreeAndNil(FMutex);
  inherited;
end;

// Add domains from OPS configuration file "filename"
// if "domain" == '', all domains will be added otherwise only the specified "domain"
// Returns true if at least one domain added
function TOPSConfigRepository.Add(filename, domainID: string): Boolean;
var
  i : Integer;
  config : TOPSConfig;
  domains : TOPSConfig.TDynDomainArray;
begin
  Result := False;

  FMutex.Acquire;
  try
    if domainID <> '' then begin
      // Check if domain already exist
      if domainExist( domainID ) then begin
        uOps.Error.gStaticErrorService.Report(TBasicError.Create(
          'TOPSConfigRepository', 'Add', 'domain "' + domainID + '" already exist'));
        Exit;
      end;
    end;

    try
      // Check if file already read
      if FConfigFiles.ContainsKey(filename) then begin
        // Get the earlier read config
        config := FConfigFiles[filename];

      end else begin
        // Need to read file
        config := TOPSConfig.getConfig( filename );
        if not Assigned(config) then begin
          uOps.Error.gStaticErrorService.Report(TBasicError.Create(
            'TOPSConfigRepository', 'Add', 'Failed to parse file "' + filename + '"'));
          Exit;
        end;
        FConfigFiles.Add(filename, config);
      end;
    except
      uOps.Error.gStaticErrorService.Report(TBasicError.Create(
        'TOPSConfigRepository', 'Add', 'Failed to parse file "' + filename + '"'));
      Exit;
    end;

    // Get all domains read from file
    domains := config.getDomains;

    // Add the choosen one(s) to our list if not already there
    for i := 0 to High(domains) do begin
      if (domainID = '') or (string(domains[i].DomainID) = domainID) then begin
        if domainExist( string(domains[i].DomainID) ) then begin
          uOps.Error.gStaticErrorService.Report(TBasicError.Create(
            'TOPSConfigRepository', 'Add', 'domain "' + string(domains[i].DomainID) + '" already exist'));
        end else begin
          // Add unique domains to our list
          (FConfig as TExtendedOPSConfig).Add(domains[i]);
          Result := True;
        end;
      end;
    end;
  finally
    FMutex.Release;
  end;
end;

procedure TOPSConfigRepository.Clear;
begin
  FMutex.Acquire;
  try
    // Since we just borrow the references to domains (they are owned by file cache)
    // we can just clear the domain list in our OPSConfig object
    (FConfig as TExtendedOPSConfig).Clear;
  finally
    FMutex.Release;
  end;
end;

function TOPSConfigRepository.domainExist(domainID: string): Boolean;
var
  i : Integer;
  domains : TOPSConfig.TDynDomainArray;
begin
  Result := False;
  FMutex.Acquire;
  try
    domains := FConfig.getDomains;
    for i := 0 to High(domains) do begin
      if string(domains[i].DomainID) = domainID then Result := True;
    end;
  finally
    FMutex.Release;
  end;
end;

function TOPSConfigRepository.getConfig(domainID: string): TOPSConfig;
begin
  Result := nil;
  FMutex.Acquire;
  try
    // If no domain have been added, we try to add the default file
    // This is for backward compatibility
    if Length(FConfig.getDomains) = 0 then begin
      if not Add('ops_config.xml') then Exit;
    end;

    if domainID <> '' then begin
      if not domainExist( domainID ) then Exit;
    end;

    Result := FConfig;
  finally
    FMutex.Release;
  end;
end;

class function TOPSConfigRepository.Instance: TOPSConfigRepository;
begin
  Result := gInstance;
end;

initialization
  gInstance := TOPSConfigRepository.Create;

finalization
  FreeAndNil(gInstance);

end.

