unit uOps.Transport.ReceiverFactory;

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
     uOps.Domain,
     uOps.Transport.Receiver;

type
  TReceiverFactory = class(TObject)
  public
    // Creates a receiver based on topic transport information
    class function getReceiver(top : TTopic; dom : TDomain) : TReceiver;
  end;

implementation

uses SysUtils;

class function TReceiverFactory.getReceiver(top : TTopic; dom : TDomain) : TReceiver;
var
  localif : string;
begin
  Result := nil;

  localIf := string(TDomain.doSubnetTranslation(dom.LocalInterface));

  if top.Transport = TTopic.TRANSPORT_MC then begin
    Result := TReceiver.createMCReceiver(string(top.DomainAddress), top.Port, localIf, top.InSocketBufferSize);
  end else if top.Transport = TTopic.TRANSPORT_TCP then begin
// Result := TReceiver.createTCPClient(top.DomainAddress, top.Port, top.InSocketBufferSize);
  end else if top.Transport = TTopic.TRANSPORT_UDP then begin
// Result := TReceiver.createUDPReceiver(0, localIf, top.InSocketBufferSize);
  end;
end;

end.

