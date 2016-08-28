unit uOps.Transport.SendDataHandlerFactory;

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
     uOps.Types,
     uOps.Error,
     uOps.Topic,
     uOps.Domain,
     uOps.Transport.Sender,
     uOps.Transport.SendDataHandler;

type
  // Method prototype to call when we connect/disconnect UDP topics with the participant info data listener
  TOnUdpConnectDisconnectProc = procedure(top : TTopic; sdh : TSendDataHandler; connect : Boolean) of object;

  TSendDataHandlerFactory = class(TObject)
  private
    // Borrowed reference
    FErrorService : TErrorService;

    // The Domain to which this Factory belongs (NOTE: we don't own the object)
    FDomain : TDomain;

    // Method to call when we connect/disconnect UDP topics with the participant info data listener
    // normally handled by the participant
    FOnUdpConnectDisconnectProc : TOnUdpConnectDisconnectProc;

    // There is only one McUdpSendDataHandler for each participant
    FUdpSendDataHandler : TSendDataHandler;

    FSendDataHandlers : TDictionary<string,TSendDataHandler>;
    FMutex : TMutex;

  public
    constructor Create(Dom : TDomain; Proc : TOnUdpConnectDisconnectProc; Reporter : TErrorService);
    destructor Destroy; override;

    function getSendDataHandler(top : TTopic) : TSendDataHandler;
    procedure releaseSendDataHandler(top : TTopic);
  end;

implementation

uses SysUtils,
     uOps.Transport.McSendDataHandler,
     uOps.Transport.McUdpSendDataHandler,
     uOps.Transport.TCPSendDataHandler;

{ TSendDataHandlerFactory }

constructor TSendDataHandlerFactory.Create(
              Dom : TDomain;
              Proc : TOnUdpConnectDisconnectProc;
              Reporter : TErrorService);
begin
  inherited Create;
  FErrorService := Reporter;
  FDomain := dom;
  FOnUdpConnectDisconnectProc := Proc;
  FMutex := TMutex.Create;
  FSendDataHandlers := TDictionary<string,TSendDataHandler>.Create;
end;

destructor TSendDataHandlerFactory.Destroy;
begin
  // Cleanup/Free all senddatahandlers under protection
  FMutex.Acquire;
  try
    // TODO

  finally
    FMutex.Release;
  end;
  FreeAndNil(FSendDataHandlers);
  FreeAndNil(FMutex);
  inherited;
end;

function TSendDataHandlerFactory.getSendDataHandler(top: TTopic): TSendDataHandler;
var
  key : string;
  localIf : string;
  ttl : Integer;
begin
  Result := nil;

  // We need to store SendDataHandlers with more than just the name as key.
  // Since topics can use the same port, we need to return the same SendDataHandler.
  // Make a key with the transport info that uniquely defines the receiver.
  key := string(top.Transport) + '::' + string(top.DomainAddress) + '::' + IntToStr(top.Port);

  FMutex.Acquire;
  try
		if FSendDataHandlers.ContainsKey(key) then begin
			Result := FSendDataHandlers.Items[key];
      Exit;
		end;

    localIf := string(TDomain.doSubnetTranslation(FDomain.LocalInterface));
    ttl := FDomain.TimeToLive;

		if top.Transport = TTopic.TRANSPORT_MC then begin
      Result := TMcSendDataHandler.Create(top, localIf, ttl, FErrorService);
      FSendDataHandlers.Add(key, Result);

    end else if top.Transport = TTopic.TRANSPORT_UDP then begin
      if not Assigned(FUdpSendDataHandler) then begin
      // We have only one sender for all topics, so use the domain value for buffer size
        FUdpSendDataHandler := TMcUdpSendDataHandler.Create(localIf,
                                 ttl,
                                 FDomain.OutSocketBufferSize,
                                 FErrorService);
			end;

			// Setup a listener on the participant info data published by participants on our domain.
			// We use the information for topics with UDP as transport, to know the destination for UDP sends
			// ie. we extract ip and port from the information and add it to our McUdpSendDataHandler
      if Assigned(FOnUdpConnectDisconnectProc) then FOnUdpConnectDisconnectProc(top, FUdpSendDataHandler, True);

      Result := FUdpSendDataHandler;

		end else if top.Transport = TTopic.TRANSPORT_TCP then begin
      Result := TTCPSendDataHandler.Create(top, FErrorService);
      FSendDataHandlers.Add(key, Result);

		end;
  finally
    FMutex.Release;
  end;
end;

procedure TSendDataHandlerFactory.releaseSendDataHandler(top: TTopic);
begin
  FMutex.Acquire;
  try
    if top.Transport = TTopic.TRANSPORT_UDP then begin
      if Assigned(FOnUdpConnectDisconnectProc) then FOnUdpConnectDisconnectProc(top, FUdpSendDataHandler, False);
		end;

///TODO

  finally
    FMutex.Release;
  end;
end;

end.

