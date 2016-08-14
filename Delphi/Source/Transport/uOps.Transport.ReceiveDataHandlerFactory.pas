unit uOps.Transport.ReceiveDataHandlerFactory;

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
     uNotifier,
     uOps.Types,
     uOps.Topic,
     uOps.MemoryMap,
     uOps.OPSMessage,
     uOps.Domain,
     uOps.SerializableInheritingTypeFactory,
     uOps.Transport.ReceiveDataHandler;

type
  TReceiveDataHandlerFactory = class(TObject)
  private
    // By Singelton, one ReceiveDataHandler per 'key' on this Participant
    FReceiveDataHandlerInstances : TDictionary<string,TReceiveDataHandler>;

    // Garbage vector for ReceiveDataHandlers, these can safely be deleted.
    FGarbageReceiveDataHandlers : TObjectList<TReceiveDataHandler>;
    FGarbageLock : TMutex;

		function makeKey(top : TTopic) : string;

  public
    constructor Create;
    destructor Destroy; override;

    function getReceiveDataHandler(
              top : TTopic;
              dom : TDomain;
              opsObjectFactory : TSerializableInheritingTypeFactory) : TReceiveDataHandler;
    procedure releaseReceiveDataHandler(top : TTopic);

    procedure cleanUpReceiveDataHandlers;
		function cleanUpDone : Boolean;

  end;

implementation

uses SysUtils;

constructor TReceiveDataHandlerFactory.Create;
begin
  inherited Create;
  FReceiveDataHandlerInstances := TDictionary<string,TReceiveDataHandler>.Create;
  FGarbageLock := TMutex.Create;
  FGarbageReceiveDataHandlers := TObjectList<TReceiveDataHandler>.Create;
end;

destructor TReceiveDataHandlerFactory.Destroy;
begin
  FreeAndNil(FGarbageReceiveDataHandlers);
  FreeAndNil(FGarbageLock);
  FreeAndNil(FReceiveDataHandlerInstances);
  inherited;
end;

function TReceiveDataHandlerFactory.makeKey(top : TTopic) : string;
begin
  // Since topics can use the same port for transports multicast & tcp, or
  // use transport udp which always use a single ReceiveDataHandler,
  // we need to return the same ReceiveDataHandler in these cases.
  // Make a key with the transport info that uniquely defines the receiver.
  if top.Transport = TTopic.TRANSPORT_UDP then begin
    Result := string(top.Transport);
  end else begin
    Result := string(top.Transport) + '::' + string(top.DomainAddress) + '::' + IntToStr(top.Port);
  end;
end;

function TReceiveDataHandlerFactory.getReceiveDataHandler(
          top : TTopic;
          dom : TDomain;
          opsObjectFactory : TSerializableInheritingTypeFactory) : TReceiveDataHandler;
var
  key : string;
  mess : string;
begin
  Result := nil;

  // Make a key with the transport info that uniquely defines the receiver.
  key := makeKey(top);

  FGarbageLock.Acquire;
  try
		if FReceiveDataHandlerInstances.ContainsKey(key) then begin
      // If we already have a ReceiveDataHandler for this topic, use it.
      Result := FReceiveDataHandlerInstances.Items[key];

      // Check if any of the topics have a sample size larger than MAX_SEGMENT_SIZE
      // This will lead to a problem when using the same port or using UDP, if samples becomes > MAX_SEGMENT_SIZE
      if ((Result.SampleMaxSize > PACKET_MAX_SIZE) or (top.SampleMaxSize > PACKET_MAX_SIZE)) then begin
        if top.Transport = TTopic.TRANSPORT_UDP then begin
          mess := 'Warning: UDP Transport is used with Topics with "sampleMaxSize" > ' + IntToStr(PACKET_MAX_SIZE);
        end else begin
          mess := 'Warning: Same port (' + IntToStr(top.Port) +
                  ') is used with Topics with "sampleMaxSize" > ' + IntToStr(PACKET_MAX_SIZE);
        end;
//      	BasicError err("ReceiveDataHandlerFactory", "getReceiveDataHandler", myMessage.str());
// 				participant->reportError(&err);
      end;

    end else if (top.Transport = TTopic.TRANSPORT_MC) or (top.Transport = TTopic.TRANSPORT_TCP) then begin
      Result := TReceiveDataHandler.Create(top, dom, opsObjectFactory);
      FReceiveDataHandlerInstances.Add(key, Result);

    end else if top.Transport = TTopic.TRANSPORT_UDP then begin
      Result := TReceiveDataHandler.Create(top, dom, opsObjectFactory);

//			participant->setUdpTransportInfo(
//				((UDPReceiver*) udpReceiveDataHandler->getReceiver())->getAddress(),
//				((UDPReceiver*) udpReceiveDataHandler->getReceiver())->getPort() );

      FReceiveDataHandlerInstances.Add(key, Result);

    end else begin // For now we can not handle more transports
      // Signal an error by returning NULL.
//			BasicError err("ReceiveDataHandlerFactory", "getReceiveDataHandler", "Unknown transport for Topic: " + top.getName());
//			participant->reportError(&err);

    end;
  finally
    FGarbageLock.Release;
  end;
end;

procedure TReceiveDataHandlerFactory.releaseReceiveDataHandler(top : TTopic);
var
  key : string;
  rdh : TReceiveDataHandler;
begin
  // Make a key with the transport info that uniquely defines the receiver.
  key := makeKey(top);

  FGarbageLock.Acquire;
  try
		if FReceiveDataHandlerInstances.ContainsKey(key) then begin
      rdh := FReceiveDataHandlerInstances.Items[key];
      if rdh.NumListeners = 0 then begin
        // Time to mark this receiveDataHandler as garbage.
        FReceiveDataHandlerInstances.Remove(key);

        rdh.Stop;

				if top.Transport = TTopic.TRANSPORT_UDP then begin
//          FParticipant->setUdpTransportInfo("", 0);
				end;

        FGarbageReceiveDataHandlers.Add(rdh);
      end;
    end;
  finally
    FGarbageLock.Release;
  end;
end;

procedure TReceiveDataHandlerFactory.cleanUpReceiveDataHandlers;
begin
  FGarbageLock.Acquire;
  try
//        for (int i = (int)garbageReceiveDataHandlers.size() - 1; i >= 0; i--)
//        {
//            if (garbageReceiveDataHandlers[i]->numReservedMessages() == 0)
//            {
//                delete garbageReceiveDataHandlers[i];
//                std::vector<ReceiveDataHandler*>::iterator iter = garbageReceiveDataHandlers.begin() + i;
//                garbageReceiveDataHandlers.erase(iter);
//            }
//        }
  finally
    FGarbageLock.Release;
  end;
end;

function TReceiveDataHandlerFactory.cleanUpDone : Boolean;
begin
  FGarbageLock.Acquire;
  try
    Result := FGarbageReceiveDataHandlers.Count = 0;
  finally
    FGarbageLock.Release;
  end;
end;

end.

