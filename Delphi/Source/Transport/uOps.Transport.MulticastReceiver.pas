unit uOps.Transport.MulticastReceiver;

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
     Sockets,
     WinSock,
     uNotifier,
     uRunner,
     uOps.Types,
     uOps.Topic,
     uOps.MemoryMap,
     uOps.OPSMessage,
     uOps.Domain,
     uOps.Transport.Receiver,
     uOps.Transport.Sockets;

type
	TMulticastReceiver = class(TReceiver)
	private
    FPort : Integer;
    FIpAddress : string;
    FLocalInterface : string;
    FInSocketBufferSize : Int64;

    FSocket: TUdpSocketEx;

    // Current read buffer from user
    FBuffer : PByte;
    FBufferSize : Integer;

    // Last source for data
    FFromAddress : TSockAddr;
    FFromAddressLen : Integer;

    // Our thread running our Run() method
    FRunner : TRunner;
    FTerminated : Boolean;

    // Will by called by the FRunner thread
    procedure Run;

  public
    constructor Create(mcAddress : string; bindPort : Integer; localInterface : string = '0.0.0.0'; inSocketBufferSize : Int64 = 16000000);
    destructor Destroy; override;

    // Start():
    // Starts the receiver, and reads bytes into given buffer.
    // When a message is read, a callback (notification) will be done with the
    // buffer and actual number of bytes read.
    // When the callback returns a new read is started to the current buffer
    procedure Start(bytes : PByte; size : Integer); override;

    // GetSource():
    // Used to get the sender IP and port for a received message.
    // Should only be called from the callback.
    procedure GetSource(var address : string; var port : Integer); override;

    // SetReceiveBuffer():
    // Changes the current buffer to use for reads.
    // Should only be called from the callback.
    procedure SetReceiveBuffer(bytes : PByte; size : Integer); override;

    // Stop():
    // Aborts an ongoing read. NOTE: Must NOT be called from the callback.
    procedure Stop; override;

    //
    function ReceiveMessage(o: PByte; size: Integer; var fromAddr : TSockAddr; var len : Integer): Integer;

    function available : Boolean;

    property Port : Integer read FPort;
    property Address : string read FIpAddress;
  end;

implementation

uses SysUtils,
     uOps.Exceptions;

constructor TMulticastReceiver.Create(mcAddress : string; bindPort : Integer; localInterface : string; inSocketBufferSize : Int64);
begin
  inherited Create;

  FPort := bindPort;
  FIpaddress := mcAddress;
  FLocalInterface := localInterface;
  FInSocketBufferSize := inSocketBufferSize;
end;

destructor TMulticastReceiver.Destroy;
begin
	Stop;   // Make sure socket is closed
  inherited;
end;

// Override from Receiver
procedure TMulticastReceiver.Start(bytes : PByte; size : Integer);
var
  localip : string;
begin
  if Assigned(FSocket) then Exit;

  FTerminated := False;
  FBuffer := bytes;
  FBufferSize := size;

  FSocket := TUdpSocketEx.Create(nil);
  FSocket.BlockMode := bmBlocking;
  FSocket.LocalHost := '0.0.0.0';
  FSocket.LocalPort := AnsiString(IntToStr(FPort));
  FSocket.Active := True;

  if FSocket.Handle = INVALID_SOCKET then begin
    raise ECommException.Create('Socket could not be opened');
  end;

  FSocket.SetReuseAddress(True);

  if not FSocket.Bind then begin
    raise ECommException.Create('Socket Bind failed');
  end;

  // Get actual port that socket is bound to (in case bindport = 0)
  FSocket.GetLocalAddr(localip, FPort);

  if FInSocketBufferSize > 0 then begin
    FSocket.SetReceiveBufferSize(Integer(FInSocketBufferSize));
    if FSocket.GetReceiveBufferSize <> Integer(FInSocketBufferSize) then begin
      // TTransportError.Create('UDPSender', 'Open', 'Socket buffer size could not be set');
      // TParticipant::reportStaticError(&err);
    end;
  end;

  FSocket.AddMulticastMembership(FIpAddress, FLocalInterface);

  if Assigned(FBuffer) then begin
    // Start a thread running our run() method
    FRunner := TRunner.Create(Run);
  end;
end;

// Override from Receiver
// Used to get the sender IP and port for a received message
// Only safe to call in callback
procedure TMulticastReceiver.getSource(var address : string; var port : Integer);
begin
  address := TUDPSocketEx.getIpAddress(FFromAddress);
  port := TUDPSocketEx.getPort(FFromAddress);
end;

// Override from Receiver
// Only safe to call in callback
procedure TMulticastReceiver.SetReceiveBuffer(bytes : PByte; size : Integer);
begin
  FBuffer := bytes;
  FBufferSize := size;
end;

// Override from Receiver
procedure TMulticastReceiver.Stop;
begin
  if not Assigned(FSocket) then Exit;

  // Tell thread to terminate
  FTerminated := True;
  if Assigned(FRunner) then FRunner.Terminate;

  shutdown(FSocket.Handle, SD_BOTH);
  FSocket.Active := False;

  // If thread exist, wait for thread to terminate and then delete the object
  FreeAndNil(FRunner);

  FreeAndNil(FSocket);

  FBuffer := nil;
  FBufferSize := 0;
end;

function TMulticastReceiver.available : Boolean;
begin
  Result := Assigned(FSocket);
end;

(**************************************************************************
*
**************************************************************************)
function TMulticastReceiver.ReceiveMessage(o: PByte; size: Integer; var fromAddr : TSockAddr; var len : Integer): Integer;
//var
//  lastError : Integer;
begin
  len := SizeOf(fromAddr);
  Result := FSocket.ReceiveFrom(o^, size, fromAddr, len);

  if Result = SOCKET_ERROR then begin
//    lastError := WSAGetLastError;
//    if lastError = WSAEWOULDBLOCK then begin
//      raise ECommException.Create('Exception at ops::UDPReceiver::ReceiveMessage: WSAWOULDBLOCK.');
//
//    end else if lastError = WSAECONNRESET then begin
//      raise ECommException.Create('Exception at ops::UDPReceiver::ReceiveMessage: WSAWCONNRESET.');
//
//    end else begin
//      raise ECommException.Create('Exception at ops::UDPReceiver::ReceiveMessage: Unknown socket error ' +
//                                  IntToStr(lastError));
//    end;
  end;
end;

(**************************************************************************
*
**************************************************************************)
procedure TMulticastReceiver.Run;
var
  Res : Integer;
begin
  while not FTerminated do begin
    Res := ReceiveMessage(FBuffer, FBufferSize, FFromAddress, FFromAddressLen);
    if FTerminated then Break;
    if Res > 0 then begin
      FDataNotifier.DoNotify(TBytesSizePair.Create(FBuffer, Res));

    end else begin
      ///TODO
//			if(error.value() == BREAK_COMM_ERROR_CODE)
//			{
//				//Communcation has been canceled from stop, do not scedule new receive
//				return;
//			}
//
//			//notifyNewEvent(data);
//			//printf("___________handleReadError__________ %d\n", error.value());
//			ops::BasicError err("MulticastReceiver", "handleReadError", "Error");
//			Participant::reportStaticError(&err);
//
//			//WSAEFAULT (10014) "Illegal buffer address" is fatal, happens e.g. if a too small buffer is given and
//			// it probably wont go away by calling the same again, so just report error and then exit without
//			// starting a new async_receive().
//			if (error.value() == WSAEFAULT) return;
//
//			asynchWait(data, max_length);
    end;
  end;
end;

//		int receive(char* buf, int size)
//		{
//			try
//			{
//				size_t nReceived = sock->receive_from(boost::asio::buffer(buf, size), lastEndpoint);
//				return (int)nReceived;
//			}
//			catch(...)
//			{
//				ops::BasicError err("MulticastReceiver", "receive", "Exception in MulticastReceiver::receive()");
//				Participant::reportStaticError(&err);
//				return -1;
//			}
//		}

//		bool sendReply(char* buf, int size)
//		{
//			try
//			{
//				sock->send_to(boost::asio::buffer(buf, size), lastEndpoint);
//				return true;
//			}
//			catch (...)
//			{
//				return false;
//			}
//		}

end.

