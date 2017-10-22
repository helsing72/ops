unit uOps.Transport.ReceiveDataHandlerFactory;

(**
*
* Copyright (C) 2016-2017 Lennart Andersson.
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
     uNotifier,
     uOps.Types,
     uOps.Error,
     uOps.Topic,
     uOps.MemoryMap,
     uOps.OPSMessage,
     uOps.Domain,
     uOps.SerializableInheritingTypeFactory,
     uOps.Transport.ReceiveDataHandler;

type
  // Method prototype to call when we want to set UDP transport info for the participant info data
  TOnUdpTransportInfoProc = procedure(ipaddress : string; port : Integer) of object;

  TReceiveDataHandlerFactory = class(TObject)
  private
    // Borrowed references
    FOnUdpTransportInfoProc : TOnUdpTransportInfoProc;
    FErrorService : TErrorService;

    // By Singelton, one ReceiveDataHandler per 'key' on this Participant
    FReceiveDataHandlerInstances : TDictionary<string,TReceiveDataHandler>;
    FLock : TMutex;

		function makeKey(top : TTopic) : string;

  public
    constructor Create(Proc : TOnUdpTransportInfoProc; Reporter : TErrorService);
    destructor Destroy; override;

    function getReceiveDataHandler(
              top : TTopic;
              dom : TDomain;
              opsObjectFactory : TSerializableInheritingTypeFactory) : TReceiveDataHandler;
    procedure releaseReceiveDataHandler(top : TTopic);

  end;

implementation

uses SysUtils,
     uOps.NetworkSupport,
     uOps.Transport.UDPReceiver;

constructor TReceiveDataHandlerFactory.Create(Proc : TOnUdpTransportInfoProc; Reporter : TErrorService);
begin
  inherited Create;
  FOnUdpTransportInfoProc := Proc;
  FErrorService := Reporter;
  FReceiveDataHandlerInstances := TDictionary<string,TReceiveDataHandler>.Create;
  FLock := TMutex.Create;
end;

destructor TReceiveDataHandlerFactory.Destroy;
begin
  FreeAndNil(FLock);
  FreeAndNil(FReceiveDataHandlerInstances);
  inherited;
end;

function TReceiveDataHandlerFactory.makeKey(top : TTopic) : string;
begin
  // Since topics can use the same port for transports multicast & tcp, or
  // use transport udp which in most cases use a single ReceiveDataHandler,
  // we need to return the same ReceiveDataHandler in these cases.
  // Make a key with the transport info that uniquely defines the receiver.
  if (top.Transport = TTopic.TRANSPORT_UDP) and (not isMyNodeAddress(top.DomainAddress)) then begin
    Result := string(top.Transport);
  end else begin
    Result := string(top.Transport) + '::' + string(top.DomainAddress) + '::' + IntToStr(top.Port);
  end;
end;

function TReceiveDataHandlerFactory.getReceiveDataHandler(
          top : TTopic;
          dom : TDomain;
          opsObjectFactory : TSerializableInheritingTypeFactory) : TReceiveDataHandler;

  procedure Report(msg : string);
  begin
    if Assigned(FErrorService) then begin
      FErrorService.Report(TBasicError.Create('ReceiveDataHandlerFactory', 'getReceiveDataHandler', msg));
    end;
  end;

var
  key : string;
begin
  Result := nil;

  // Make a key with the transport info that uniquely defines the receiver.
  key := makeKey(top);

  FLock.Acquire;
  try
		if FReceiveDataHandlerInstances.ContainsKey(key) then begin
      // If we already have a ReceiveDataHandler for this topic, use it.
      Result := FReceiveDataHandlerInstances.Items[key];

      // Check if any of the topics have a sample size larger than MAX_SEGMENT_SIZE
      // This will lead to a problem when using the same port or using UDP, if samples becomes > MAX_SEGMENT_SIZE
      if ((Result.SampleMaxSize > PACKET_MAX_SIZE) or (top.SampleMaxSize > PACKET_MAX_SIZE)) then begin
        if top.Transport = TTopic.TRANSPORT_UDP then begin
          Report('Warning: UDP Transport is used with Topics with "sampleMaxSize" > ' + IntToStr(PACKET_MAX_SIZE));
        end else begin
          Report('Warning: Same port (' + IntToStr(top.Port) +
                 ') is used with Topics with "sampleMaxSize" > ' + IntToStr(PACKET_MAX_SIZE));
        end;
      end;

    end else if (top.Transport = TTopic.TRANSPORT_MC) or (top.Transport = TTopic.TRANSPORT_TCP) then begin
      Result := TReceiveDataHandler.Create(top, dom, opsObjectFactory, FErrorService);
      FReceiveDataHandlerInstances.Add(key, Result);

    end else if top.Transport = TTopic.TRANSPORT_UDP then begin
      Result := TReceiveDataHandler.Create(top, dom, opsObjectFactory, FErrorService);

      if key = string(top.Transport) then begin
        if Assigned(FOnUdpTransportInfoProc) then begin
          FOnUdpTransportInfoProc(
            (Result.getReceiver as TUDPReceiver).Address,
            (Result.getReceiver as TUDPReceiver).Port);
        end;
      end;

      FReceiveDataHandlerInstances.Add(key, Result);

    end else begin // For now we can not handle more transports
      Report('Unknown transport for Topic: ' + string(top.Name));
      // Signal an error by returning nil.

    end;
  finally
    FLock.Release;
  end;
end;

procedure TReceiveDataHandlerFactory.releaseReceiveDataHandler(top : TTopic);
var
  key : string;
  rdh : TReceiveDataHandler;
begin
  // Make a key with the transport info that uniquely defines the receiver.
  key := makeKey(top);

  FLock.Acquire;
  try
		if FReceiveDataHandlerInstances.ContainsKey(key) then begin
      rdh := FReceiveDataHandlerInstances.Items[key];
      if rdh.NumListeners = 0 then begin
        // Start by removing it from the active list
        FReceiveDataHandlerInstances.Remove(key);

        FreeAndNil(rdh);

				if top.Transport = TTopic.TRANSPORT_UDP then begin
          if key = string(top.Transport) then begin
            if Assigned(FOnUdpTransportInfoProc) then begin
              FOnUdpTransportInfoProc('', 0);
            end;
          end;
				end;
      end;
    end;
  finally
    FLock.Release;
  end;
end;

end.

