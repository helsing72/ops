unit uOps.Transport.Sender;

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

uses uOps.Error;

type
  // Interface used to send data
  TSender = class(TObject)
  protected
    // Borrowed reference
    FErrorService : TErrorService;

    // Result from WSAGetLastError() on error
    FLastErrorCode : Integer;

  public
    function sendTo(buf : PByte; size : Integer; ip : string; port : Integer) : Boolean; virtual; abstract;
    function getPort() : Integer; virtual; abstract;
    function getAddress() : string; virtual; abstract;
    procedure Open(); virtual; abstract;
    procedure Close(); virtual; abstract;

    property ErrorService : TErrorService read FErrorService write FErrorService;
    property LastErrorCode : Integer read FLastErrorCode;
  end;

  (* ------------------------------------------------------------------------ *)

  TSenderFactory = class(TObject)
  public
    // Creates a receiver
    class function createMCSender(localInterface : string = '0.0.0.0'; ttl : Integer = 1; outSocketBufferSize : Int64 = 16000000) : TSender;
    class function createUDPSender(localInterface : string = '0.0.0.0'; ttl : Integer = 1; outSocketBufferSize : Int64 = 16000000) : TSender;
    class function createTCPServer(ip : string; port : Integer; outSocketBufferSize : Int64 = 16000000) : TSender;
  end;

implementation

uses uOps.Transport.UDPSender,
     uOps.Transport.TCPServer;

class function TSenderFactory.createMCSender(localInterface : string; ttl : Integer; outSocketBufferSize : Int64) : TSender;
begin
  Result := TUDPSender.Create(localInterface, ttl, outSocketBufferSize, True);
end;

class function TSenderFactory.createUDPSender(localInterface : string; ttl : Integer; outSocketBufferSize : Int64) : TSender;
begin
  Result := TUDPSender.Create(localInterface, ttl, outSocketBufferSize, False);
end;

class function TSenderFactory.createTCPServer(ip : string; port : Integer; outSocketBufferSize : Int64) : TSender;
begin
  Result := TTCPServerSender.Create(ip, port, outSocketBufferSize);
end;

end.

