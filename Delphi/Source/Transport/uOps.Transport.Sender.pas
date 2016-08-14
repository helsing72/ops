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

type
  // Interface used to send data
  TSender = class(TObject)
  public
    function sendTo(buf : PByte; size : Integer; ip : string; port : Integer) : Boolean; virtual; abstract;
    ///function receiveReply(buf : PByte; size : Integer) : Integer; virtual; abstract;
    function getPort() : Integer; virtual; abstract;
    function getAddress() : string; virtual; abstract;
    procedure Open(); virtual; abstract;
    procedure Close(); virtual; abstract;

    class function createMCSender(localInterface : string = '0.0.0.0'; ttl : Integer = 1; outSocketBufferSize : Int64 = 16000000) : TSender;
    class function createUDPSender(localInterface : string = '0.0.0.0'; ttl : Integer = 1; outSocketBufferSize : Int64 = 16000000) : TSender;
    class function createTCPServer(ip : string; port : Integer; outSocketBufferSize : Int64 = 16000000) : TSender;
  end;

implementation

uses uOps.Transport.UDPSender;

class function TSender.createMCSender(localInterface : string; ttl : Integer; outSocketBufferSize : Int64) : TSender;
begin
  Result := TUDPSender.Create(localInterface, ttl, outSocketBufferSize, True);
end;

class function TSender.createUDPSender(localInterface : string; ttl : Integer; outSocketBufferSize : Int64) : TSender;
begin
  Result := TUDPSender.Create(localInterface, ttl, outSocketBufferSize, False);
end;

class function TSender.createTCPServer(ip : string; port : Integer; outSocketBufferSize : Int64) : TSender;
begin
  Result := nil;
		///LA Can't have a static storage of TCPServers with only port as key,
		/// this will not work if the same port is used on two different IP addresses.
		/// Besides, there is already a store of TCPSendDataHandlers,
		/// so when we come here we should always create a new TCPServer

        //static std::map<int, TCPServer*> tcpSenderInstances;

        //TCPServer* newInstance = NULL;
        //if (tcpSenderInstances.find(port) == tcpSenderInstances.end())
        //{
        //    newInstance = new TCPServer(ip, port, ioService);
        //    tcpSenderInstances[port] = newInstance;
        //}
        //return tcpSenderInstances[port];

//		return new TCPServer(ip, port, ioService, outSocketBufferSize);
end;

end.

