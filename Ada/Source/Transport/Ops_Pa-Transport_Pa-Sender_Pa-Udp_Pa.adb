--
-- Copyright (C) 2016 Lennart Andersson.
--
-- This file is part of OPS (Open Publish Subscribe).
--
-- OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--
-- You should have received a copy of the GNU Lesser General Public License
-- along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.

with Ada.Unchecked_Conversion;
with Interfaces.C;
with Interfaces.C.Strings;
with Win32;

package body Ops_Pa.Transport_Pa.Sender_Pa.Udp_Pa is

  use type Win32.INT;
  use type Win32.Winsock.SOCKET;

  function To_PSTR is new Ada.Unchecked_Conversion(System.Address, Win32.PSTR);

  function ToAddr is new Ada.Unchecked_Conversion(Win32.Winsock.SOCKADDR_IN, Win32.Winsock.SOCKADDR);
  function ToAddr is new Ada.Unchecked_Conversion(Win32.Winsock.SOCKADDR, Win32.Winsock.SOCKADDR_IN);

  function MakeSockAddr(ip : String; port : Integer) return Win32.Winsock.SOCKADDR_IN is
    addr : Win32.Winsock.SOCKADDR_IN;
  begin
    addr.sin_family := Win32.Winsock.AF_INET;
    --function htons(hostshort: u_short): u_short; stdcall;
    addr.sin_port := Win32.Winsock.htons(Win32.Winsock.u_short(Port));
    --function inet_addr(cp: PAnsiChar): u_long; stdcall;
    addr.sin_addr.S_Un.s_addr := Win32.Winsock.inet_addr(Win32.Addr(ip & ASCII.NUL));
    addr.sin_zero := (others => Interfaces.C.nul);
    return addr;
  end;

  -- Constructs a new UDPSender and binds its underlying socket to local host
  -- and a dynamically allocated local port.
  -- This class accepts synchronous write operations through sendTo().
  function Create(localInterface : String := "0.0.0.0";
                  ttl : Integer := 1;
                  outSocketBufferSize : Int64 := 16000000;
                  multicastSocket : Boolean := False) return UdpSender_Class_At is
     Self : UdpSender_Class_At := null;
  begin
    Self := new UdpSender_Class;
    InitInstance( Self.all, localInterface, ttl, outSocketBufferSize, multicastSocket );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  procedure InitInstance( Self : in out UdpSender_Class;
                          localInterface : String;
                          ttl : Integer;
                          outSocketBufferSize : Int64;
                          multicastSocket : Boolean ) is
  begin
    Self.LocalInterface := Copy(localInterface);
    Self.Ttl := ttl;
    Self.OutSocketBufferSize := outSocketBufferSize;
    Self.MulticastSocket := multicastSocket;
    Open( Self );
  end;

  procedure Finalize( Self : in out UdpSender_Class ) is
  begin
    Close( Self );

    if Self.LocalInterface /= null then
      Dispose(Self.LocalInterface);
    end if;
  end;

  procedure Report( Self : in out UdpSender_Class; method : string; mess : string) is
    error : SocketError_Class_At := null;
  begin
    if Self.ErrorService /= null then
      error := SocketError("UDPSender", method, mess, Self.LastErrorCode);
      Self.ErrorService.Report(Error_Class_At(error));
    end if;
  end;

  overriding procedure Open( Self : in out UdpSender_Class ) is
    NonBlock : aliased Win32.ULONG := 1;
    OptVal : UInt32;
    OptLen : aliased Win32.INT;
    addr : aliased Win32.winsock.SOCKADDR := ToAddr(MakeSockAddr("0.0.0.0", 0));
  begin
    Self.LastErrorCode := 0;
    if Self.SocketId /= Win32.Winsock.INVALID_SOCKET then
      return;
    end if;

    Self.SocketId := Win32.Winsock.socket_func(Win32.Winsock.AF_INET, Win32.Winsock.SOCK_DGRAM, Win32.Winsock.IPPROTO_UDP);

    if Self.SocketId = Win32.Winsock.INVALID_SOCKET then
      Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      Report(Self, "Open", "Socket could not be created");
      return;
    end if;

    --function ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer; stdcall;
    if Win32.Winsock.ioctlsocket(Self.SocketId, Win32.Winsock.FIONBIO, NonBlock'Access) = Win32.Winsock.SOCKET_ERROR then
      Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
    end if;

    if Self.OutSocketBufferSize > 0 then
      OptVal := UInt32(Self.OutSocketBufferSize);
      OptLen := 4;
      --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
      if Win32.Winsock.setsockopt(self.SocketId,
                                  Win32.Winsock.SOL_SOCKET,
                                  Win32.Winsock.SO_SNDBUF,
                                  Win32.To_PCSTR(OptVal'Address),
                                  OptLen) = Win32.Winsock.SOCKET_ERROR
      then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      end if;

      OptVal := 0;
      OptLen := 4;
      --function getsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; var optlen: Integer): Integer; stdcall;
      if Win32.Winsock.getsockopt(Self.SocketId,
                                  Win32.Winsock.SOL_SOCKET,
                                  Win32.Winsock.SO_SNDBUF,
                                  To_PSTR(OptVal'Address),
                                  OptLen'Access) = Win32.Winsock.SOCKET_ERROR
      then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      end if;

      if OptVal /= UINT32(Self.OutSocketBufferSize) then
        if Self.LastErrorCode = 0 then
          Self.LastErrorCode := Win32.Winsock.SOCKET_ERROR;
        end if;
        Report(Self, "Open", "Socket buffer size could not be set");
      end if;
    end if;

    if Self.MulticastSocket then
      OptVal := UInt32(Self.Ttl);
      OptLen := 4;
      --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
      if Win32.Winsock.setsockopt(Self.SocketId,
                                  Win32.Winsock.IPPROTO_IP,
                                  Win32.Winsock.IP_MULTICAST_TTL,
                                  Win32.To_PCSTR(OptVal'Address),
                                  OptLen) = Win32.Winsock.SOCKET_ERROR
      then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      end if;

      OptVal := UInt32(Win32.winsock.inet_addr( Win32.Addr( Self.LocalInterface.all )));
      OptLen := 4;
      --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
      if Win32.Winsock.setsockopt(Self.SocketId,
                                  Win32.Winsock.IPPROTO_IP,
                                  Win32.Winsock.IP_MULTICAST_IF,
                                  Win32.To_PCSTR(OptVal'Address),
                                  OptLen) = Win32.Winsock.SOCKET_ERROR
      then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      end if;
    end if;

    --function bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer; stdcall;
    if Win32.WinSock.bind(Self.SocketId, addr'Unchecked_Access, 16) = Win32.Winsock.SOCKET_ERROR then
      Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
    end if;
  end;

  overriding procedure Close( Self : in out UdpSender_Class ) is
    res : Win32.INT;
    SD_BOTH : Win32.INT := 2;
  begin
    Self.LastErrorCode := 0;
    if Self.SocketId /= Win32.Winsock.INVALID_SOCKET then
      --function shutdown(s: TSocket; how: Integer): Integer; stdcall;
      res := Win32.Winsock.shutdown(Self.SocketId, SD_BOTH);

      --function closesocket(s: TSocket): Integer; stdcall;
      if Win32.Winsock.closesocket(Self.SocketId) = Win32.Winsock.SOCKET_ERROR then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      end if;

      Self.SocketId := Win32.Winsock.INVALID_SOCKET;
    end if;
  end;

  overriding function getAddress( Self : in out UdpSender_Class ) return String is
    Address : aliased Win32.Winsock.SOCKADDR;
    AddressLen : aliased Win32.INT := 16;
  begin
    -- function getsockname(s : SOCKET; name : access SOCKADDR; namelen : access Win32.INT) return Win32.INT;
    if Win32.Winsock.getsockname( s => Self.SocketId,
                                  name => Address'Access,
                                  namelen => AddressLen'Access) = Win32.Winsock.SOCKET_ERROR then
      Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      Report(Self, "getAddress", "Failed to get bound address");
      return "";
    else
      return Interfaces.C.Strings.Value(Win32.To_Chars_Ptr(Win32.Winsock.inet_ntoa( ToAddr(Address).sin_addr )));
    end if;
  end;

  overriding function getPort( Self : in out UdpSender_Class ) return Integer is
    Address : aliased Win32.Winsock.SOCKADDR;
    AddressLen : aliased Win32.INT := 16;
  begin
    -- function getsockname(s : SOCKET; name : access SOCKADDR; namelen : access Win32.INT) return Win32.INT;
    if Win32.Winsock.getsockname( s => Self.SocketId,
                                  name => Address'Access,
                                  namelen => AddressLen'Access) = Win32.Winsock.SOCKET_ERROR then
      Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      Report(Self, "getPort", "Failed to get bound port");
      return 0;
    else
      return Integer(Win32.Winsock.ntohs(ToAddr(Address).sin_port));
    end if;
  end;

  overriding function sendTo( Self : in out UdpSender_Class; buf : Byte_Arr_At; size : Integer; ip : string; port : Integer) return Boolean is
    DstAddr : aliased Win32.Winsock.SOCKADDR := ToAddr(MakeSockAddr(ip, port));
  begin
    Self.LastErrorCode := 0;
    if Self.SocketId = Win32.Winsock.INVALID_SOCKET then
      return False;
    end if;

    --    function To_PCSTR is new Ada.Unchecked_Conversion (System.Address, PCSTR);

    --      function sendto
    --       (s     : SOCKET;
    --        buf   : Win32.PCSTR;
    --        len   : Win32.INT;
    --        flags : Win32.INT;
    --        to    : ac_SOCKADDR_t;
    --        tolen : Win32.INT)
    --        return Win32.INT;
    if Win32.WinSock.sendto(Self.SocketId,
                            Win32.to_PCSTR(buf.all'Address),
                            Win32.INT(size),
                            0,
                            DstAddr'Unchecked_Access,
                            16) = Win32.Winsock.SOCKET_ERROR
    then
      Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      Report(Self, "sendTo", "sendto() failed");
      return False;
    end if;

    return True;
  end;

  WSA : Win32.INT;
  wVersionRequired : Win32.WORD := 16#0101#;
  WSAData : aliased Win32.Winsock.WSADATA;
begin
  WSA := Win32.Winsock.WSAStartup( wVersionRequired, WSAData'Unchecked_Access );

end Ops_Pa.Transport_Pa.Sender_Pa.Udp_Pa;

