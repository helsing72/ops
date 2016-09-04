unit uOps.OPSConfig;

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
     uOps.Types,
     uOps.Topic,
     uOps.Domain,
     uOps.OpsObject,
     uOps.ArchiverInOut;

type
	TOPSConfig = class(TOPSObject)
  public
    type
      TDynDomainArray = array of TDomain;
	private
    FDomains : TDynDomainArray;

	public
    constructor Create;
    destructor Destroy; override;

    class function getConfig : TOPSConfig; overload;
    class function getConfig(configFile : string) : TOPSConfig; overload;

    class procedure releaseConfig(var cfg : TOPSConfig);

    // Returns a reference to the given Domain
    // NOTE: The OPSConfig still owns it
    function getDomain(domainID : string) : TDomain; virtual;

    // Returns references to the internal Domains
    // NOTE: The OPSConfig still owns them
    function getDomains : TDynDomainArray;

    procedure Serialize(archiver : TArchiverInOut); override;

		// Returns a newely allocated deep copy/clone of this object.
		function Clone : TOPSObject; override;

		// Fills the parameter obj with all values from this object.
		procedure FillClone(var obj : TOPSObject); override;
	end;

  TDefaultOPSConfigImpl = class(TOPSConfig)
  public
    constructor Create;
    procedure Serialize(archiver : TArchiverInOut); override;
  end;

implementation

uses Classes,
     SysUtils,
     System.SyncObjs,
     uOps.OPSObjectFactory,
     uOps.XMLArchiverIn;

constructor TOPSConfig.Create;
begin
  inherited Create;
end;

destructor TOPSConfig.Destroy;
var
  i : Integer;
begin
  // Delete all domains
  for i := 0 to Length(FDomains)-1 do FreeAndNil(FDomains[i]);
  inherited;
end;

function TOPSConfig.getDomain(domainID : string) : TDomain;
var
  i : Integer;
begin
  Result := nil;
  for i := 0 to Length(FDomains) - 1 do begin
    if string(FDomains[i].DomainID) = domainID then begin
      Result := FDomains[i];
      Break;
    end;
  end;
end;

function TOPSConfig.getDomains : TDynDomainArray;
begin
  Result := Copy(FDomains);
end;

procedure TOPSConfig.Serialize(archiver : TArchiverInOut);
begin
  inherited Serialize(archiver);

	archiver.inout('domains', TDynSerializableArray(FDomains));
end;

// Returns a newely allocated deep copy/clone of this object.
function TOPSConfig.Clone : TOPSObject;
begin
	Result := TOPSConfig.Create;
  Self.FillClone(Result);
end;

// Fills the parameter obj with all values from this object.
procedure TOPSConfig.FillClone(var obj : TOPSObject);
var
  i : Integer;
begin
	inherited FillClone(obj);
  with obj as TOPSConfig do begin
    SetLength(FDomains, Length(Self.FDomains));
    for i := 0 to High(Self.FDomains) do begin
      FDomains[i] := Self.FDomains[i].Clone as TDomain;
    end;
  end;
end;

// ---------------------------------------------------------------------------

constructor TDefaultOPSConfigImpl.Create;
begin
  inherited Create;
  AppendType('DefaultOPSConfigImpl');
end;

procedure TDefaultOPSConfigImpl.Serialize(archiver : TArchiverInOut);
begin
  inherited Serialize(archiver);
end;

// ---------------------------------------------------------------------------

var
  gConfiguration : TOPSConfig;
  gMutex : TMutex;

class function TOPSConfig.getConfig(configFile : string) : TOPSConfig;
var
  archiver : TXMLArchiverIn;
  list : TStringList;
begin
  Result := nil;

  list := TStringList.Create;
  try
    list.LoadFromFile(configFile);
    archiver := nil;
    try
      archiver := TXMLArchiverIn.Create(list.Text, 'root', TOPSObjectFactory.getInstance);
      Result := TOPSConfig(archiver.inout2('ops_config', TSerializable(Result)));
    finally
      FreeAndNil(archiver);
    end;
  finally
    FreeAndNil(list);
  end;
end;

class function TOPSConfig.getConfig : TOPSConfig;
begin
  // Protection
  gMutex.Acquire;
  try
  	if not Assigned(gConfiguration) then begin
      gConfiguration := getConfig('ops_config.xml');
    end;
  finally
    gMutex.Release;
  end;
  Result := gConfiguration;
end;

class procedure TOPSConfig.releaseConfig(var cfg : TOPSConfig);
begin
  // We don't want to delete the singleton instance
  if cfg <> gConfiguration then FreeAndNil(cfg);
  cfg := nil;
end;

initialization
  gConfiguration := nil;
  gMutex := TMutex.Create;

finalization
  FreeAndNil(gConfiguration);
  FreeAndNil(gMutex);

end.

