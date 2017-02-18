unit uOps.Domain;

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

uses uOps.Types,
     uOps.Topic,
     uOps.Channel,
     uOps.Transport,
     uOps.ArchiverInOut,
     uOps.OpsObject;

type
	TDomain = class(TOPSObject)
  public
    type
      TDynTopicArray = array of TTopic;
      TDynChannelArray = array of TChannel;
      TDynTransportArray = array of TTransport;
	private
		FDomainAddress : AnsiString;
		FTimeToLive : Integer;
		FLocalInterface : AnsiString;
		FInSocketBufferSize : Integer;
		FOutSocketBufferSize : Integer;
    FTopics : TDynTopicArray;
		FDomainID : AnsiString;
		FMetaDataMcPort : Integer;

		FChannels : TDynChannelArray;
		FTransports : TDynTransportArray;

		procedure checkTopicValues(top : TTopic);
		procedure checkTransports;
		function findChannel(id : AnsiString ) : TChannel;
		function findTopic(id : AnsiString) : TTopic;

	public
    constructor Create;
    destructor Destroy; override;

    // Returns references to the internal topics
    // NOTE: The Domain still owns them
    function getTopics : TDynTopicArray;

    // Returns a reference to the internal topic
    // NOTE: The Domain still owns it
    function getTopic(Name : AnsiString) : TTopic;

		function topicExist(Name : AnsiString) : Boolean;

    procedure Serialize(archiver : TArchiverInOut); override;

		// Returns a newely allocated deep copy/clone of this object.
		function Clone : TOPSObject; override;

		// Fills the parameter obj with all values from this object.
		procedure FillClone(var obj : TOPSObject); override;

		class function doSubnetTranslation(addr : AnsiString) : AnsiString;

		property DomainAddress : AnsiString read FDomainAddress;
		property DomainID : AnsiString read FDomainID;
		property MetaDataMcPort : Integer read FMetaDataMcPort;
    property TimeToLive : Integer read FTimeToLive;
    property LocalInterface : AnsiString read FLocalInterface;
		property InSocketBufferSize : Integer read FInSocketBufferSize;
		property OutSocketBufferSize : Integer read FOutSocketBufferSize;
  end;

implementation

uses SysUtils,
     Winapi.Windows,
     Winapi.IpHlpApi,
     Winapi.IpRtrMib,
     Winapi.Winsock,
     uOps.Exceptions,
     uOps.XMLArchiverIn;

constructor TDomain.Create;
begin
  inherited;
	FTimeToLive := 1;
	FLocalInterface := '0.0.0.0';
	FInSocketBufferSize := -1;    // Use OS default, Topics may override
	FOutSocketBufferSize := -1;   // Use OS default, Topics may override
	FMetaDataMcPort := 9494;      // Default port
	AppendType('Domain');
end;

destructor TDomain.Destroy;
var
  i : Integer;
begin
  // Delete all topics
  for i := 0 to Length(FTopics)-1 do FreeAndNil(FTopics[i]);
  inherited;
end;

procedure TDomain.checkTopicValues(top : TTopic);
begin
	if top.DomainAddress = '' then top.DomainAddress := FDomainAddress;
  if top.LocalInterface = '' then top.LocalInterface := FLocalInterface;
	if top.TimeToLive < 0 then top.TimeToLive := timeToLive;
	if top.InSocketBufferSize < 0 then top.InSocketBufferSize := FInSocketBufferSize;
	if top.OutSocketBufferSize < 0 then	top.OutSocketBufferSize := FOutSocketBufferSize;
end;

// Returns references to the internal topics
function TDomain.getTopics : TDynTopicArray;
var
  top : TTopic;
begin
  for top in FTopics do checkTopicValues(top);
  Result := Copy(FTopics);
end;

// Returns a reference to the internal topic
function TDomain.getTopic(Name : AnsiString) : TTopic;
var
  i : Integer;
begin
	for i := 0 to Length(FTopics)-1 do begin
		if FTopics[i].Name = Name then begin
			checkTopicValues(FTopics[i]);
			Result := FTopics[i];
      Exit;
		end;
	end;
	raise ENoSuchTopicException.Create('Topic ' + string(Name) + ' does not exist in ops config file.');
end;

function TDomain.topicExist(Name : AnsiString) : Boolean;
var
  i : Integer;
begin
  Result := False;
	for i := 0 to Length(FTopics)-1 do begin
		if FTopics[i].Name = Name then begin
			Result := True;
      Break;
		end;
	end;
end;

procedure TDomain.Serialize(archiver : TArchiverInOut);
begin
	inherited Serialize(archiver);
	archiver.inout('domainID', FDomainID);
	archiver.inout('topics', TDynSerializableArray(FTopics));
	archiver.inout('domainAddress', FDomainAddress);
	archiver.inout('localInterface', FLocalInterface);
	archiver.inout('timeToLive', FTimeToLive);
	archiver.inout('inSocketBufferSize', FInSocketBufferSize);
	archiver.inout('outSocketBufferSize', FOutSocketBufferSize);
	archiver.inout('metaDataMcPort', FMetaDataMcPort);

  // To not break binary compatibility we only do this when we know we are
  // reading from an XML-file
  if archiver is TXMLArchiverIn then begin
		archiver.inout('channels', TDynSerializableArray(FChannels));
		archiver.inout('transports', TDynSerializableArray(FTransports));
		CheckTransports();
  end;
 end;

// Returns a newely allocated deep copy/clone of this object.
function TDomain.Clone : TOPSObject;
begin
	Result := TDomain.Create;
  Self.FillClone(Result);
end;

// Fills the parameter obj with all values from this object.
procedure TDomain.FillClone(var obj : TOPSObject);
var
  i : Integer;
begin
	inherited FillClone(obj);
  with obj as TDomain do begin
		FDomainAddress := Self.FDomainAddress;
		FTimeToLive := Self.FTimeToLive;
		FLocalInterface := Self.FLocalInterface;
		FInSocketBufferSize := Self.FInSocketBufferSize;
		FOutSocketBufferSize := Self.FOutSocketBufferSize;

    SetLength(FTopics, Length(Self.FTopics));
    for i := 0 to High(Self.FTopics) do begin
      FTopics[i] := Self.FTopics[i].Clone as TTopic;
    end;

		FDomainID := Self.FDomainID;
		FMetaDataMcPort := Self.FMetaDataMcPort;

    SetLength(FChannels, Length(Self.FChannels));
    for i := 0 to High(Self.FChannels) do begin
      FChannels[i] := Self.FChannels[i].Clone as TChannel;
    end;

    SetLength(FTransports, Length(Self.FTransports));
    for i := 0 to High(Self.FTransports) do begin
      FTransports[i] := Self.FTransports[i].Clone as TTransport;
    end;
  end;
end;

function TDomain.findChannel(id : AnsiString) : TChannel;
var
  i : Integer;
begin
  Result := nil;
	if id <> '' then begin
		for i := 0 to High(FChannels) do begin
			if id = FChannels[i].channelID then begin
        Result := FChannels[i];
        Break;
      end;
		end;
	end;
end;

function TDomain.findTopic(id : AnsiString) : TTopic;
var
  i : Integer;
begin
  Result := nil;
	if id <> '' then begin
		for i := 0 to High(FTopics) do begin
			if id = FTopics[i].Name then begin
        Result := FTopics[i];
        Break;
      end;
		end;
	end;
end;

procedure TDomain.checkTransports;
var
  i, j : Integer;
  channel : TChannel;
  top : TTopic;
begin
	// Now update topics with values from the transports and channels
	// Loop over all transports and for each topic, see if it needs parameters from the channel
	for i := 0 to High(FTransports) do begin
		// Get channel
		channel := findChannel(FTransports[i].channelID);
		if not Assigned(channel) then begin
			raise EConfigException.Create(
				'Non existing channelID: "' + string(FTransports[i].channelID) +
				'" used in transport spcification.');
		end else begin
			for j := 0 to High(FTransports[i].topics) do begin
				top := findTopic(FTransports[i].topics[j]);
				if not Assigned(top) then begin
					raise EConfigException(
						'Non existing topicID: "' + FTransports[i].topics[j] +
						'" used in transport spcification.');
				end else begin
					channel.PopulateTopic(top);
				end;
			end;
		end;
	end;
end;

/// ------------------------------------------
/// Helper to get all IP interfaces

procedure VVGetIpAddrTable(var p: PMibIpAddrTable; var Size: Cardinal; const bOrder: BOOL);
var
  Res: DWORD;
begin
  p := nil;
  Size := 0;
  if @GetIpAddrTable = nil then Exit;   //Not implemented in this windows version
  Res := GetIpAddrTable(p,Size,bOrder);
  if Res=ERROR_INSUFFICIENT_BUFFER then begin
    Getmem(p,Size);
    // Caller must free this buffer when it is no longer used
    FillChar(p^,Size,#0);
    Res := GetIpAddrTable(p,Size,bOrder);
  end;
  if Res <> NO_ERROR then begin
    if Assigned(p) then FreeMem(p);
    p := nil;
    Size := 0;
  end;
end;

function IpAddressToString(Addr: DWORD): AnsiString;
var
  InAddr: TInAddr;
begin
  InAddr.S_addr := Addr;
  Result := AnsiString(inet_ntoa(InAddr));
end;

// If argument contains a "/" we assume it is on the form:  subnet-address/subnet-mask
// e.g "192.168.10.0/255.255.255.0" or "192.168.10.0/24"
// In that case we loop over all interfaces and take the first one that matches
// i.e. the one whos interface address is on the subnet
class function TDomain.doSubnetTranslation(addr : AnsiString) : AnsiString;
var
  Idx : Integer;
  subnet, mask : AnsiString;
  subnetIp, subnetMask : DWORD;
  Size: ULONG;
  p: PMibIpAddrTable;
  i: integer;
begin
  Result := addr;

  // If no '/' we just return the given address
  Idx := Pos('/', string(addr));
  if Idx = 0 then Exit;

  subnet := Copy(addr, 1, Idx-1);
  mask   := Copy(addr, Idx+1, MaxInt);

  subnetIp := inet_addr(PAnsiChar(subnet));
  if Length(mask) <= 2 then begin
    // Expand to the number of bits given
    subnetMask := StrToInt(string(mask));
    subnetMask := (((1 shl subnetMask)-1) shl (32 - subnetMask)) and $FFFFFFFF;
    subnetMask := ntohl(subnetMask);
  end else begin
    subnetMask := inet_addr(PAnsiChar(mask));
  end;

  VVGetIpAddrTable(p, Size, False);
  if Assigned(p) then begin
    try
      with p^ do begin
        for i := 0 to dwNumEntries - 1 do begin
          with table[i] do begin
            if (dwAddr and subnetMask) = (subnetIp and subnetMask) then begin
              Result := IpAddressToString(dwAddr);
              Break;
            end;
          end;
        end;
      end;
    finally
      FreeMem(p);
    end;
  end;
end;

end.


