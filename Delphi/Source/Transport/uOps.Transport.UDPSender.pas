unit uOps.Transport.UDPSender;

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

uses uOps.Transport.Sender,
     uOps.Transport.Sockets;

type
  // A sender implementation that dispatches messages over ip based UDP.
  TUDPSender = class(TSender)
  private
    FLocalInterface : string;
    FTtl : Integer;
    FOutSocketBufferSize : Int64;
    FMulticastSocket : Boolean;

    FSocket: TUdpSocketEx;

  public
    // Constructs a new UDPSender and binds its underlying socket to local host
    // and a dynamically allocated local port.
		// This class accepts synchronous write operations through sendTo().
    constructor Create(localInterface : string = '0.0.0.0';
                       ttl : Integer = 1;
                       outSocketBufferSize : Int64 = 16000000;
                       multicastSocket : Boolean = False);
    destructor Destroy; override;

    procedure Open(); override;
    procedure Close(); override;

    function sendTo(buf : PByte; size : Integer; ip : string; port : Integer) : Boolean; override;

    function getPort() : Integer; override;
    function getAddress() : string; override;

    ///function receiveReply(buf : PByte; size : Integer) : Integer; override;
    ///function waitForReply(timeoutMS : Integer) : Boolean;
  end;

implementation

uses SysUtils,
     WinSock,
     Sockets,
     uOps.Exceptions,
     uOps.Transport.Error;

{ TUDPSender }

constructor TUDPSender.Create(localInterface: string; ttl: Integer;
  outSocketBufferSize: Int64; multicastSocket: Boolean);
begin
  inherited Create;

  FLocalInterface := localInterface;
  FTtl := ttl;
  FOutSocketBufferSize := outSocketBufferSize;
  FMulticastSocket := multicastSocket;

  Open;
end;

destructor TUDPSender.Destroy;
begin
  Close;
  inherited;
end;

procedure TUDPSender.Open;
begin
  if Assigned(FSocket) then Exit;

  FSocket := TUdpSocketEx.Create(nil);
  FSocket.BlockMode := bmNonBlocking;
  FSocket.Active := True;

  if FSocket.Handle = INVALID_SOCKET then begin
    raise ECommException.Create('Socket could not be opened');
    // TTransportError.Create('UDPSender', 'Open', 'Socket could not be created');
    // TParticipant.reportStaticError(&err);
  end;

  if FOutSocketBufferSize > 0 then begin
    FSocket.SetSendBufferSize(Integer(FOutSocketBufferSize));
    if FSocket.GetSendBufferSize <> Integer(FOutSocketBufferSize) then begin
      // TTransportError.Create('UDPSender', 'Open', 'Socket buffer size could not be set');
      // TParticipant::reportStaticError(&err);
    end;
  end;

  if FMulticastSocket then begin
    FSocket.SetMulticastTtl(FTtl);
    FSocket.SetMulticastInterface(FLocalInterface);
  end;

  FSocket.Bind;
end;

procedure TUDPSender.Close;
begin
  if Assigned(FSocket) then shutdown(FSocket.Handle, SD_BOTH);
  FreeAndNil(FSocket);
end;

function TUDPSender.getAddress: string;
var
  dummy : Integer;
begin
  Result := '';
  if not Assigned(FSocket) then Exit;
  dummy := 0;
  FSocket.GetLocalAddr(Result, dummy);
end;

function TUDPSender.getPort: Integer;
var
  dummy : string;
begin
  Result := 0;
  if not Assigned(FSocket) then Exit;
  FSocket.GetLocalAddr(dummy, Result);
end;

function TUDPSender.sendTo(buf: PByte; size: Integer; ip: string; port: Integer): Boolean;
var
  ToAddr : TSockAddrIn;
begin
  Result := False;
  if not Assigned(FSocket) then Exit;

  try
    ToAddr := FSocket.GetSocketAddr(AnsiString(ip), AnsiString(IntToStr(port)));
    if FSocket.SendTo(buf^, size, ToAddr) = SOCKET_ERROR then begin
      raise ECommException.Create('Exception at ops::UDPSender::SendMessageTo: sendto() failed');
    end;

    Result := True;

  except
//		catch (std::exception& ex)
//        {
//			std::stringstream ss;
//			ss << "Error when sending udp message: " << ex.what() << " Params: buf = " << buf << ", size = " << size << ", ip = " << ip << ", port = " << port;
//			ops::BasicError err("UDPSender", "sendTo", ss.str());
//			Participant::reportStaticError(&err);
//            return false;
//        }
//        catch (...)
//        {
//			std::stringstream ss;
//			ss << "Error when sending udp message: Params: buf = " << buf << ", size = " << size << ", ip = " << ip << ", port = " << port;
//			ops::BasicError err("UDPSender", "sendTo", ss.str());
//			Participant::reportStaticError(&err);
//            return false;
//        }
  end;
end;

//function TUDPSender.receiveReply(buf: PByte; size: Integer): Integer;
//begin
//		if (!socket) return 0;
//		size_t nReceived = socket->receive(boost::asio::buffer(buf, size));
//		return (int)nReceived;
//end;

//function TUDPSender.waitForReply(int timeout) : Boolean;
//begin
//		/*__int64 startWait = TimeHelper::currentTimeMillis();
//		while(TimeHelper::currentTimeMillis())*/
//		return false;
//end;

end.

