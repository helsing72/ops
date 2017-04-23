--
-- Copyright (C) 2016-2017 Lennart Andersson.
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

package body Ops_Pa.Transport_Pa.Receiver_Pa.MulticastReceiver_Pa is

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

  function Create( mcAddress : string;
                   bindPort : Integer;
                   localInterface : string := "0.0.0.0";
                   inSocketBufferSize : Int64 := 16000000 ) return MulticastReceiver_Class_At is
     Self : MulticastReceiver_Class_At := null;
  begin
    Self := new MulticastReceiver_Class;
    InitInstance( Self.all, Self, mcAddress, bindPort, localInterface, inSocketBufferSize );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  procedure InitInstance( Self : in out MulticastReceiver_Class;
                          SelfAt : MulticastReceiver_Class_At;
                          mcAddress : string;
                          bindPort : Integer;
                          localInterface : string;
                          inSocketBufferSize : Int64) is
  begin
    InitInstance( Receiver_Class(Self), Receiver_Class_At(SelfAt) );
    Self.Port := bindPort;
    Self.Ipaddress := Copy(mcAddress);
    Self.LocalInterface := Copy(localInterface);
    Self.InSocketBufferSize := inSocketBufferSize;
  end;

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out MulticastReceiver_Class ) is
  begin
    Stop( Self );   -- Make sure socket is closed

    if Self.IpAddress /= null then
      Dispose(Self.IpAddress);
    end if;
    if Self.LocalInterface /= null then
      Dispose(Self.LocalInterface);
    end if;

    Finalize( Receiver_Class(Self) );
  end;

  procedure Report( Self : in out MulticastReceiver_Class; method : string; mess : string) is
    error : SocketError_Class_At := null;
  begin
    if Self.ErrorService /= null then
      error := SocketError("MulticastReceiver", method, mess, Self.LastErrorCode);
      Self.ErrorService.Report(Error_Class_At(error));
    end if;
  end;

  -- Override from Receiver
  overriding function Start( Self: in out MulticastReceiver_Class; bytes : Byte_Arr_At; size : Integer) return Boolean is
    OptVal : UInt32;
    OptLen : aliased Win32.INT;
  begin
    Self.LastErrorCode := 0;
    if Self.SocketId /= Win32.Winsock.INVALID_SOCKET then
      return True;
    end if;

    Self.StopFlag := False;
    Self.Buffer := bytes;
    Self.BufferSize := size;

    -- Create socket
    Self.SocketId := Win32.Winsock.socket_func(Win32.Winsock.AF_INET, Win32.Winsock.SOCK_DGRAM, Win32.Winsock.IPPROTO_UDP);

    if Self.SocketId = Win32.Winsock.INVALID_SOCKET then
      Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      Report(Self, "Open", "Socket could not be created");
      return False;
    end if;

    -- Set blocking calls
    declare
      NonBlock : aliased Win32.ULONG := 0;
    begin
      --function ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer; stdcall;
      if Win32.Winsock.ioctlsocket(Self.SocketId, Win32.Winsock.FIONBIO, NonBlock'Access) = Win32.Winsock.SOCKET_ERROR then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      end if;
    end;

    -- Set reuse address
    OptVal := 1;
    OptLen := 4;
    --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
    if Win32.Winsock.setsockopt(self.SocketId,
                                Win32.Winsock.SOL_SOCKET,
                                Win32.Winsock.SO_REUSEADDR,
                                Win32.To_PCSTR(OptVal'Address),
                                OptLen) = Win32.Winsock.SOCKET_ERROR
    then
      Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      Report(Self, "Start", "Failed to set REUSEADDR");
    end if;

    -- Bind socket to local address
    declare
      addr : aliased Win32.winsock.SOCKADDR := ToAddr(MakeSockAddr("0.0.0.0", Self.Port));
    begin
      --function bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer; stdcall;
      if Win32.WinSock.bind(Self.SocketId, addr'Unchecked_Access, 16) = Win32.Winsock.SOCKET_ERROR then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
        Report(Self, "Start", "Bind error");
        return False;
      end if;
    end;

    -- Get actual port that socket is bound to (in case bind port = 0)
    declare
      Address : aliased Win32.Winsock.SOCKADDR;
      AddressLen : aliased Win32.INT := 16;
    begin
      -- function getsockname(s : SOCKET; name : access SOCKADDR; namelen : access Win32.INT) return Win32.INT;
      if Win32.Winsock.getsockname( s => Self.SocketId,
                                    name => Address'Access,
                                    namelen => AddressLen'Access) = Win32.Winsock.SOCKET_ERROR then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
        Report(Self, "Start", "Failed to get bound port");
      else
        Self.Port := Integer(Win32.Winsock.ntohs(ToAddr(Address).sin_port));
      end if;
    end;

    -- Set socket buffer size
    if Self.InSocketBufferSize > 0 then
      OptVal := UInt32(Self.InSocketBufferSize);
      OptLen := 4;
      --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
      if Win32.Winsock.setsockopt(self.SocketId,
                                  Win32.Winsock.SOL_SOCKET,
                                  Win32.Winsock.SO_RCVBUF,
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
                                  Win32.Winsock.SO_RCVBUF,
                                  To_PSTR(OptVal'Address),
                                  OptLen'Access) = Win32.Winsock.SOCKET_ERROR
      then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      end if;

      if OptVal /= UINT32(Self.InSocketBufferSize) then
        if Self.LastErrorCode = 0 then
          Self.LastErrorCode := Win32.Winsock.SOCKET_ERROR;
        end if;
        Report(Self, "Start", "Socket buffer size could not be set");
      end if;
    end if;

    -- Join the Multicast group
    declare
      OptVal : Win32.Winsock.ip_mreq;
      addr : Win32.Winsock.IN_ADDR;
      ifc : Win32.Winsock.IN_ADDR;

      -- When linking with Ws2_32.lib the IP_ADD_MEMBERSHIP should be = 12 instead of 5 (Win32.Winsock.IP_ADD_MEMBERSHIP)
      IP_ADD_MEMBERSHIP : constant := 12;
    begin
      -- FSocket.AddMulticastMembership(AnsiString(FIpAddress), AnsiString(Self.LocalInterface));
      addr.S_Un.s_addr :=  Win32.Winsock.inet_addr(Win32.Addr(Self.IpAddress.all & ASCII.NUL));
      ifc.S_Un.s_addr := Win32.Winsock.inet_addr(Win32.Addr(Self.LocalInterface.all & ASCII.NUL));
      OptVal.imr_multiaddr := addr;
      OptVal.imr_interface := ifc;
      OptLen := 8;
      if Win32.Winsock.setsockopt(self.SocketId,
                                  Win32.Winsock.IPPROTO_IP,
                                  IP_ADD_MEMBERSHIP,
                                  Win32.To_PCSTR(OptVal'Address),
                                  OptLen) = Win32.Winsock.SOCKET_ERROR
      then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
        Report(Self, "Start", "Failed to join Multicast Group");
      end if;
    end;

    if Self.Buffer /= null then
      -- Start a thread running our run() method
      Self.EventsToTask.Signal(StartEvent_C);
    end if;
    return True;
  end;

  -- Override from Receiver
  -- Used to get the sender IP and port for a received message
  -- Only safe to call in callback
  overriding function GetSourceIP( Self : in out MulticastReceiver_Class ) return String is
    addr : Win32.Winsock.SOCKADDR_IN := ToAddr(Self.FromAddress);
  begin
    --function inet_ntoa(inaddr: TInAddr): PAnsiChar; stdcall;
    return Interfaces.C.Strings.Value(Win32.To_Chars_Ptr(Win32.Winsock.inet_ntoa( c_in => addr.sin_addr )));
  end;

  overriding function GetSourcePort( Self : in out MulticastReceiver_Class ) return Integer is
    addr : Win32.Winsock.SOCKADDR_IN := ToAddr(Self.FromAddress);
  begin
    return Integer(Win32.Winsock.ntohs(addr.sin_port));
  end;

  -- Override from Receiver
  -- Only safe to call in callback
  overriding procedure SetReceiveBuffer( Self : in out MulticastReceiver_Class; bytes : Byte_Arr_At; size : Integer) is
  begin
    Self.Buffer := bytes;
    Self.BufferSize := size;
  end;

  -- Override from Receiver
  overriding procedure Stop( Self : in out MulticastReceiver_Class ) is
    res : Win32.INT;
    SD_BOTH : Win32.INT := 2;
  begin
    if Self.SocketId /= Win32.Winsock.INVALID_SOCKET then
      -- Tell run to exit
      Self.StopFlag := True;

      --function shutdown(s: TSocket; how: Integer): Integer; stdcall;
      res := Win32.Winsock.shutdown(Self.SocketId, SD_BOTH);

      --function closesocket(s: TSocket): Integer; stdcall;
      if Win32.Winsock.closesocket(Self.SocketId) = Win32.Winsock.SOCKET_ERROR then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      end if;

      Self.SocketId := Win32.Winsock.INVALID_SOCKET;

      Self.Buffer := null;
      Self.BufferSize := 0;
    end if;
  end;

  function available( Self : MulticastReceiver_Class ) return Boolean is
  begin
    return Self.SocketId /= Win32.Winsock.INVALID_SOCKET;
  end;

  function Port( Self : MulticastReceiver_Class ) return Integer is
  begin
    return Self.Port;
  end;

  function Address( Self : MulticastReceiver_Class ) return String is
  begin
    if Self.IpAddress /= null then
      return Self.IpAddress.all;
    else
      return "";
    end if;
  end;

  function ReceiveMessage( Self : in out MulticastReceiver_Class; o: Byte_Arr_At; size: Integer; fromAddr : access Win32.Winsock.SOCKADDR; len : access Win32.INT) return Integer is
    res : Win32.INT;
    Result : Integer := 0;
  begin
    if Self.SocketId /= Win32.Winsock.INVALID_SOCKET then
      Self.LastErrorCode := 0;
      len.all := 16;

      -- function recvfrom(s : SOCKET; buf : Win32.PSTR; len : Win32.INT; flags : Win32.INT; from : access SOCKADDR; fromlen : access Win32.INT) return Win32.INT;
      res := Win32.Winsock.recvfrom(s       => Self.SocketId,
                                    buf     => Win32.To_PSTR(Win32.To_Chars_Ptr(Win32.To_PCSTR(o.all'address))),
                                    len     => Win32.INT(size),
                                    flags   => 0,
                                    from    => fromAddr,
                                    fromlen => len);
      if res = Win32.Winsock.SOCKET_ERROR then
        Self.LastErrorCode := Integer(Win32.Winsock.WSAGetLastError);
      end if;
      Result := Integer(res);
    end if;
    return Result;
  end;

  overriding procedure Run( Self : in out MulticastReceiver_Class ) is
    Res : Integer;
  begin
    while not Self.StopFlag loop
      Res := ReceiveMessage(Self, Self.Buffer, Self.BufferSize, Self.FromAddress'Access, Self.FromAddressLen'Access);
      exit when Self.StopFlag;

      if Res > 0 then
        -- Got some data, Notify listener
        Self.DataNotifier.DoNotify(BytesSizePair_T'(Self.Buffer, Res));

      elsif Res = 0 then
        -- Could this happen?
        delay 0.010;    -- So we don't hog the cpu on errors

      else
        if Self.LastErrorCode = Win32.Winsock.WSAEWOULDBLOCK then
          -- Can't happen since we use a blocking socket
          null;

        elsif Self.LastErrorCode = Win32.Winsock.WSAECONNRESET then
        -- On a UDP-datagram socket this error indicates a previous send
        -- operation resulted in an ICMP Port Unreachable message.
          null;

        else
          null;
        end if;

        Report(Self, "Run", "Receive error");

        delay 0.010;    -- So we don't hog the cpu on errors
      end if;
    end loop;
  end;

end Ops_Pa.Transport_Pa.Receiver_Pa.MulticastReceiver_Pa;


