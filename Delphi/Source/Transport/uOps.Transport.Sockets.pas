unit uOps.Transport.Sockets;

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

uses WinSock, Classes, Sockets;

type
(**************************************************************************
* Internal help class
*
* We can't use the TUdpSocket in Sockets.pas due to the connect handling.
* Instead our class inherit directly from TIpSocket so we avoid the connect
* handling in TCustomIpClient.
*
///* Another thing is that TIpSocket.ReceiveFrom() has the wrong parameters,
///* so we have our own corrected implementation instead.
*
* The TBaseSocket.WaitForData() don't work correctly for Datagram messages
* so we have our own implementation.
**************************************************************************)
  TUdpSocketEx = class(TIpSocket)
  public
    constructor Create(AOwner: TComponent); override;

    // Make Bind visible
    function Bind: Boolean;
    procedure GetLocalAddr(var localip : string; var localport : Integer);

///    // The corrected prototype
///    function ReceiveFrom(var buf; bufsize: Integer; var FromAddr: TSockAddr;
///                         var FromLen: Integer; flags: Integer = 0) : Integer;

    // A corrected implementation
    function WaitForData(TimeOut: Integer): Boolean;

    procedure SetReceiveBufferSize(size : Integer);
    procedure SetSendBufferSize(size : Integer);
    function GetReceiveBufferSize: Integer;
    function GetSendBufferSize: Integer;

    procedure SetReuseAddress(value : Boolean);

    procedure SetMulticastTtl(value : Integer);
    procedure SetMulticastInterface(ifaddr : string);
    procedure AddMulticastMembership(mcaddr : string; ifaddr : string);
    procedure DropMulticastMembership(mcaddr : string; ifaddr : string);

    class function getIpAddress(addr : TSockAddr) : string;
    class function getPort(addr : TSockAddr) : Integer;

  end;

implementation

uses Windows, SysUtils, uOps.Types, uOps.Exceptions;

{ TUdpSocketEx }

(**************************************************************************
*
**************************************************************************)
constructor TUdpSocketEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  SockType := stDgram;
  Protocol := IPPROTO_UDP;
end;

(**************************************************************************
*
**************************************************************************)
function TUdpSocketEx.Bind: Boolean;
begin
  Result := inherited Bind;
end;

(**************************************************************************
*
**************************************************************************)
class function TUdpSocketEx.getIpAddress(addr : TSockAddr) : string;
begin
  Result := string(inet_ntoa(addr.sin_addr));
end;

class function TUdpSocketEx.getPort(addr : TSockAddr) : Integer;
begin
  Result := ntohs(addr.sin_port);
end;

(**************************************************************************
*
**************************************************************************)
procedure TUdpSocketEx.GetLocalAddr(var localip : string; var localport : Integer);
var
//  ec,
  len : Integer;
  addr : TSockAddr;
begin
  len := SizeOf(addr);

  //function getsockname(s: TSocket; var name: TSockAddr; var namelen: Integer): Integer; stdcall;
  if getsockname(Handle, addr, len) = SOCKET_ERROR then begin
//    ec := WSAGetLastError;
  end;

  localip := getIpAddress(addr);
  localport := getPort(addr);
end;

//(**************************************************************************
//* BUG FIX (Delphi code is wrong)
//**************************************************************************)
//function TUdpSocketEx.ReceiveFrom(var buf; bufsize: Integer; var FromAddr: TSockAddr;
//                                  var Fromlen: Integer; flags: Integer): Integer;
//begin
//  //function recvfrom(s: TSocket; var Buf; len, flags: Integer;
//  //  var from: TSockAddr; var fromlen: Integer): Integer; stdcall;
//  Result := ErrorCheck(WinSock.recvfrom(Handle, buf, bufsize, flags, FromAddr, FromLen));
//  if Result <> SOCKET_ERROR then
//    DoReceive(pansichar(@Buf), Result);
//end;

(**************************************************************************
* BUG FIX (Delphi code don't work for UDP messages)
**************************************************************************)
function TUdpSocketEx.WaitForData(TimeOut: Integer): Boolean;
var
  ReadReady, ExceptFlag: Boolean;
  Num : Integer;
begin
  Result := False;
  // Select also returns True when connection is broken.
  if Select(@ReadReady, nil, @ExceptFlag, TimeOut) then
    if ReadReady and not ExceptFlag then begin
      if ioctlsocket(Handle, FIONREAD, Num) = 0 then
        Result := Num > 0;
    end;
end;

procedure TUdpSocketEx.SetReceiveBufferSize(size : Integer);
var
  OptLen : Integer;
  OptVal : Integer;
begin
  if Handle = INVALID_SOCKET then Exit;

  OptVal := size;
  OptLen := 4;
  // function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
  setsockopt(Handle, SOL_SOCKET, SO_RCVBUF, PAnsiChar(@OptVal), OptLen);
end;

procedure TUdpSocketEx.SetSendBufferSize(size : Integer);
var
  OptLen : Integer;
  OptVal : Integer;
begin
  if Handle = INVALID_SOCKET then Exit;

  OptVal := size;
  OptLen := 4;
  // function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
  setsockopt(Handle, SOL_SOCKET, SO_SNDBUF, PAnsiChar(@OptVal), OptLen);
end;

function TUdpSocketEx.GetReceiveBufferSize: Integer;
var
  OptLen : Integer;
  OptVal : Integer;
begin
  OptVal := 0;
  if Handle <> INVALID_SOCKET then begin
    OptLen := 4;
    // function getsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; var optlen: Integer): Integer; stdcall;
    getsockopt(Handle, SOL_SOCKET, SO_RCVBUF, PAnsiChar(@OptVal), OptLen);
  end;
  Result := OptVal;
end;

function TUdpSocketEx.GetSendBufferSize: Integer;
var
  OptLen : Integer;
  OptVal : Integer;
begin
  OptVal := 0;
  if Handle <> INVALID_SOCKET then begin
    OptLen := 4;
    // function getsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; var optlen: Integer): Integer; stdcall;
    getsockopt(Handle, SOL_SOCKET, SO_SNDBUF, PAnsiChar(@OptVal), OptLen);
  end;
  Result := OptVal;
end;

procedure TUdpSocketEx.SetReuseAddress(value : Boolean);
var
  Flag : BOOL;
begin
  if Handle = INVALID_SOCKET then Exit;

  Flag := value;
  if setsockopt(Handle, SOL_SOCKET, SO_REUSEADDR, PAnsiChar(@Flag), SizeOf(Flag)) = SOCKET_ERROR then begin
    raise ECommException.Create('Failed to set SO_REUSEADDR');
  end;
end;

procedure TUdpSocketEx.SetMulticastTtl(value : Integer);
var
  OptLen : Integer;
  OptVal : Integer;
begin
  if Handle = INVALID_SOCKET then Exit;

  OptVal := value;
  OptLen := 4;
  // function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
  if setsockopt(Handle, IPPROTO_IP, IP_MULTICAST_TTL, PAnsiChar(@OptVal), OptLen) = SOCKET_ERROR then begin
    raise ECommException.Create('Failed to set TTL');
  end;
end;

procedure TUdpSocketEx.SetMulticastInterface(ifaddr : string);
var
  OptLen : Integer;
  OptVal : Integer;
  addr : TSockAddr;
//  ec : Integer;
begin
  if Handle = INVALID_SOCKET then Exit;

  addr := GetSocketAddr(AnsiString(ifaddr), '0');
  OptVal := addr.sin_addr.S_addr;
  OptLen := SizeOf(OptVal);

  // function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
  if setsockopt(Handle, IPPROTO_IP, IP_MULTICAST_IF, PAnsiChar(@OptVal), OptLen) = SOCKET_ERROR then begin
//    ec := WSAGetLastError;
    raise ECommException.Create('Failed to set multicast interface');
  end;
end;

type
  //typedef struct ip_mreq {
  //  struct in_addr  imr_multiaddr;
  //  struct in_addr  imr_interface;
  //} IP_MREQ, *PIP_MREQ;
  ip_mreq = record
    imr_multiaddr : in_addr;
    imr_interface : in_addr;
  end;

procedure TUdpSocketEx.AddMulticastMembership(mcaddr : string; ifaddr : string);
var
  OptLen : Integer;
  OptVal : ip_mreq;
  addr : TSockAddr;
//  ec : Integer;
begin
  if Handle = INVALID_SOCKET then Exit;

  addr := GetSocketAddr(AnsiString(mcaddr), '0');
  OptVal.imr_multiaddr := addr.sin_addr;

  addr := GetSocketAddr(AnsiString(ifaddr), '0');
  OptVal.imr_interface := addr.sin_addr;

  OptLen := SizeOf(OptVal);

  // function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
  if setsockopt(Handle, IPPROTO_IP, IP_ADD_MEMBERSHIP, PAnsiChar(@OptVal), OptLen) = SOCKET_ERROR then begin
//    ec := WSAGetLastError;
    raise ECommException.Create('Failed to add membership');
  end;
end;

procedure TUdpSocketEx.DropMulticastMembership(mcaddr : string; ifaddr : string);
var
  OptLen : Integer;
  OptVal : ip_mreq;
  addr : TSockAddr;
begin
  if Handle = INVALID_SOCKET then Exit;

  addr := GetSocketAddr(AnsiString(mcaddr), '0');
  OptVal.imr_multiaddr := addr.sin_addr;

  addr := GetSocketAddr(AnsiString(ifaddr), '0');
  OptVal.imr_interface := addr.sin_addr;

  OptLen := SizeOf(OptVal);

  // function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
  if setsockopt(Handle, IPPROTO_IP, IP_DROP_MEMBERSHIP, PAnsiChar(@OptVal), OptLen) = SOCKET_ERROR then begin
    raise ECommException.Create('Failed to drop membership');
  end;
end;

//int                  /* OUT: whatever setsockopt() returns */
//join_source_group(int sd, u_int32 grpaddr,
//   u_int32 srcaddr, u_int32 iaddr)
//{
//   struct ip_mreq_source imr;
//
//   imr.imr_multiaddr.s_addr  = grpaddr;
//   imr.imr_sourceaddr.s_addr = srcaddr;
//   imr.imr_interface.s_addr  = iaddr;
//   return setsockopt(sd, IPPROTO_IP, IP_ADD_SOURCE_MEMBERSHIP, (char *) &imr, sizeof(imr));
//}

end.

