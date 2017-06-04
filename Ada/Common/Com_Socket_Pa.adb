--
-- Copyright (C) 2017 Lennart Andersson.
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

with Ada.Characters.Latin_1,
     Ada.Text_IO,
     Ada.Strings.Fixed;

with Ada.Unchecked_Conversion;
with Interfaces; use Interfaces;
with Interfaces.C;
with Interfaces.C.Strings;

with Win32.Winsock;

package body Com_Socket_Pa is

  function FromSocket is new Ada.Unchecked_Conversion(Win32.Winsock.SOCKET, SocketID_T);
  function ToSocket is new Ada.Unchecked_Conversion(SocketID_T, Win32.Winsock.SOCKET);

  use type Win32.Winsock.SOCKET;
  use type Interfaces.C.int;

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

  function GetHostName return String is
    hname: String(1..1024) := (1 => Ada.Characters.Latin_1.NUL, others => ' ');
    Status : Win32.Int;
  begin
    --function gethostname(name: Win32.PSTR; namelen : Win32.INT) return Win32.INT;
    Status := Win32.Winsock.gethostname(name => Win32.Addr(hname), namelen => hname'Length);

    declare
      host : String := Ada.Strings.Fixed.Trim(hname, Ada.Strings.Both);
    begin
      -- Skip trailing nul on host name
      return host(host'First..host'Last-1);
    end;
  end;


  function Create( SocketType : Integer; Protocol : Integer )  return Socket_Class_At is
     Self : Socket_Class_At := null;
  begin
    Self := new Socket_Class;
    InitInstance( Self.all, Self, SocketType, Protocol );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out Socket_Class;
                          SelfAt : Socket_Class_At;
                          SocketType : Integer;
                          Protocol : Integer ) is
  begin
    Self.SelfAt := SelfAt;
    Self.SocketType := SocketType;
    Self.Protocol := Protocol;
  end;

  overriding procedure Finalize( Self : in out Socket_Class ) is
    dummy : Boolean;
  begin
    if Self.IsOpen then
      dummy := Self.Shutdown;
      dummy := Self.Close;
    end if;
  end;

  function Open( Self : in out Socket_Class ) return Boolean is
  begin
    Self.SocketId := FromSocket(Win32.Winsock.socket_func(Win32.Winsock.AF_INET, Interfaces.C.int(Self.SocketType), Interfaces.C.int(Self.Protocol)));
    return Self.IsOpen;
  end;

  function IsOpen( Self : in out Socket_Class ) return Boolean is
  begin
    return ToSocket(Self.SocketId) /= Win32.Winsock.INVALID_SOCKET;
  end;

  function Shutdown( Self : in out Socket_Class ) return Boolean is
    res : Win32.INT;
    SD_BOTH : Win32.INT := 2;
  begin
    if Self.IsOpen then
      --function shutdown(s: TSocket; how: Integer): Integer; stdcall;
      res := Win32.Winsock.shutdown(ToSocket(Self.SocketId), SD_BOTH);
    end if;
    return True;
  end;

  function Close( Self : in out Socket_Class ) return Boolean is
  begin
    if Self.IsOpen then
      --function closesocket(s: TSocket): Integer; stdcall;
      if Win32.Winsock.closesocket(ToSocket(Self.SocketId)) = Win32.Winsock.SOCKET_ERROR then
        return False;
      end if;
      Self.SocketId := Invalid_SocketID_C;
    end if;
    return True;
  end;

  function SocketID( Self : Socket_Class ) return SocketID_T is
  begin
    return Self.SocketID;
  end;

  function GetLatestError( Self : Socket_Class ) return Integer is
  begin
    return Integer(Win32.Winsock.WSAGetLastError);
  end;

  function SetNonBlocking( Self : in out Socket_Class; Value : Boolean ) return Boolean is
    NonBlock : aliased Win32.ULONG := 0;
  begin
    if Value then
      NonBlock := 1;
    end if;
    --function ioctlsocket(s: TSocket; cmd: DWORD; var arg: u_long): Integer; stdcall;
    if Win32.Winsock.ioctlsocket(ToSocket(Self.SocketId), Win32.Winsock.FIONBIO, NonBlock'Access) = Win32.Winsock.SOCKET_ERROR then
      return False;
    end if;
    return True;
  end;

  function SetReuseAddress( Self : in out Socket_Class; Value : Boolean ) return Boolean is
    OptVal : Win32.ULONG := 0;
    OptLen : aliased Win32.INT := 4;
  begin
    if Value then
      OptVal := 1;
    end if;
    --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
    if Win32.Winsock.setsockopt(ToSocket(Self.SocketId),
                                Win32.Winsock.SOL_SOCKET,
                                Win32.Winsock.SO_REUSEADDR,
                                Win32.To_PCSTR(OptVal'Address),
                                OptLen) = Win32.Winsock.SOCKET_ERROR
    then
      return False;
    end if;
    return True;
  end;

  function Bind( Self : in out Socket_Class; Ip : String; Port : Integer ) return Boolean is
    addr : aliased Win32.winsock.SOCKADDR := ToAddr(MakeSockAddr(Ip, Port));
  begin
    --function bind(s: TSocket; var addr: TSockAddr; namelen: Integer): Integer; stdcall;
    if Win32.WinSock.bind(Self.SocketId, addr'Unchecked_Access, 16) = Win32.Winsock.SOCKET_ERROR then
      return False;
    end if;
    return True;
  end;

  function GetBoundIP( Self : in out Socket_Class ) return String is
    Address : aliased Win32.Winsock.SOCKADDR;
    AddressLen : aliased Win32.INT := 16;
  begin
    -- function getsockname(s : SOCKET; name : access SOCKADDR; namelen : access Win32.INT) return Win32.INT;
    if Win32.Winsock.getsockname( s => Self.SocketId,
                                  name => Address'Access,
                                  namelen => AddressLen'Access) = Win32.Winsock.SOCKET_ERROR
    then
      return "";
    end if;
    return Interfaces.C.Strings.Value(Win32.To_Chars_Ptr(Win32.Winsock.inet_ntoa( ToAddr(Address).sin_addr )));
  end;

  function GetBoundPort( Self : in out Socket_Class; Port : in out Integer ) return Boolean is
    Address : aliased Win32.Winsock.SOCKADDR;
    AddressLen : aliased Win32.INT := 16;
  begin
    -- function getsockname(s : SOCKET; name : access SOCKADDR; namelen : access Win32.INT) return Win32.INT;
    if Win32.Winsock.getsockname( s => Self.SocketId,
                                  name => Address'Access,
                                  namelen => AddressLen'Access) = Win32.Winsock.SOCKET_ERROR
    then
      return False;
    end if;
    Port := Integer(Win32.Winsock.ntohs(ToAddr(Address).sin_port));
    return True;
  end;

  function SetReceiveBufferSize( Self : in out Socket_Class; Value : Integer ) return Boolean is
    OptVal : Win32.ULONG := Win32.ULONG(Value);
    OptLen : aliased Win32.INT := 4;
  begin
    --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
    if Win32.Winsock.setsockopt(ToSocket(Self.SocketId),
                                Win32.Winsock.SOL_SOCKET,
                                Win32.Winsock.SO_RCVBUF,
                                Win32.To_PCSTR(OptVal'Address),
                                OptLen) = Win32.Winsock.SOCKET_ERROR
    then
      return False;
    end if;
    return True;
  end;

  function GetReceiveBufferSize( Self : in out Socket_Class ) return Integer is
    OptVal : Win32.ULONG := 0;
    OptLen : aliased Win32.INT  := 4;
  begin
    --function getsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; var optlen: Integer): Integer; stdcall;
    if Win32.Winsock.getsockopt(ToSocket(Self.SocketId),
                                Win32.Winsock.SOL_SOCKET,
                                Win32.Winsock.SO_RCVBUF,
                                To_PSTR(OptVal'Address),
                                OptLen'Access) = Win32.Winsock.SOCKET_ERROR
    then
      return -1;
    end if;
    return Integer(OptVal);
  end;

  function SetSendBufferSize( Self : in out Socket_Class; Value : Integer ) return Boolean is
    OptVal : Win32.ULONG := win32.ULONG(Value);
    OptLen : aliased Win32.INT := 4;
  begin
    --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
    if Win32.Winsock.setsockopt(ToSocket(Self.SocketId),
                                Win32.Winsock.SOL_SOCKET,
                                Win32.Winsock.SO_SNDBUF,
                                Win32.To_PCSTR(OptVal'Address),
                                OptLen) = Win32.Winsock.SOCKET_ERROR
    then
      return False;
    end if;
    return True;
  end;

  function GetSendBufferSize( Self : in out Socket_Class ) return Integer is
    OptVal : Win32.ULONG := 0;
    OptLen : aliased Win32.INT  := 4;
  begin
    --function getsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; var optlen: Integer): Integer; stdcall;
    if Win32.Winsock.getsockopt(ToSocket(Self.SocketId),
                                Win32.Winsock.SOL_SOCKET,
                                Win32.Winsock.SO_SNDBUF,
                                To_PSTR(OptVal'Address),
                                OptLen'Access) = Win32.Winsock.SOCKET_ERROR
    then
      return -1;
    end if;
    return Integer(OptVal);
  end;

  function SendBuf( Self : in out Socket_Class; Buf : System.Address; BufSize : Integer) return Integer is
    res : Win32.INT;
  begin
    res := Win32.Winsock.send(ToSocket(Self.SocketID),
                              Win32.To_PCSTR(Buf),
                              Win32.INT(BufSize),
                              0);
    if res = Win32.Winsock.SOCKET_ERROR then
      return -1;
    end if;
    return Integer(res);
  end;

  function ReceiveBuf( Self : in out Socket_Class; Buf : System.Address; BufSize : Integer ) return Integer is
    res : Win32.INT;
  begin
    res := Win32.Winsock.recv(ToSocket(Self.SocketID),
                              To_PSTR(Buf),
                              Win32.INT(BufSize),
                              0);
    if res = Win32.Winsock.SOCKET_ERROR then
      return -1;
    end if;
    return Integer(res);
  end;

  -- Saves from address internally and it is available via API
  function ReceiveFrom( Self : in out Socket_Class; Buf : System.Address; BufSize : Integer ) return Integer is
    res : Win32.INT;
  begin
    Self.FromAddressLen := 16;

    -- function recvfrom(s : SOCKET; buf : Win32.PSTR; len : Win32.INT; flags : Win32.INT; from : access SOCKADDR; fromlen : access Win32.INT) return Win32.INT;
    res := Win32.Winsock.recvfrom(s       => Self.SocketId,
                                  buf     => To_PSTR(Buf),
                                  len     => Win32.INT(BufSize),
                                  flags   => 0,
                                  from    => Self.FromAddress'Access,
                                  fromlen => Self.FromAddressLen'Access);
    if res = Win32.Winsock.SOCKET_ERROR then
      return -1;
    end if;
    return Integer(res);
  end;

  function GetSourceIP( Self : in out Socket_Class ) return String is
    addr : Win32.Winsock.SOCKADDR_IN := ToAddr(Self.FromAddress);
  begin
    --function inet_ntoa(inaddr: TInAddr): PAnsiChar; stdcall;
    return Interfaces.C.Strings.Value(Win32.To_Chars_Ptr(Win32.Winsock.inet_ntoa( c_in => addr.sin_addr )));
  end;

  function GetSourcePort( Self : in out Socket_Class ) return Integer is
    addr : Win32.Winsock.SOCKADDR_IN := ToAddr(Self.FromAddress);
  begin
    return Integer(Win32.Winsock.ntohs(addr.sin_port));
  end;

  function SendTo( Self : in out Socket_Class; Buf : System.Address; Size : Integer; Ip : String; Port : Integer ) return Integer is
    DstAddr : aliased Win32.Winsock.SOCKADDR := ToAddr(MakeSockAddr(Ip, Port));
    res : Win32.INT := 0;
  begin
    --    function To_PCSTR is new Ada.Unchecked_Conversion (System.Address, PCSTR);

    --      function sendto
    --       (s     : SOCKET;
    --        buf   : Win32.PCSTR;
    --        len   : Win32.INT;
    --        flags : Win32.INT;
    --        to    : ac_SOCKADDR_t;
    --        tolen : Win32.INT)
    --        return Win32.INT;
    res := Win32.WinSock.sendto(ToSocket(Self.SocketId),
                                Win32.To_PCSTR(Buf),
                                Win32.INT(Size),
                                0,
                                DstAddr'Unchecked_Access,
                                16);
    if res = Win32.Winsock.SOCKET_ERROR then
      return -1;
    end if;
    return Integer(res);
  end;

  --------------------------------------------------------------------------

  function Create return UDPSocket_Class_At is
     Self : UDPSocket_Class_At := null;
  begin
    Self := new UDPSocket_Class;
    InitInstance( Self.all, Self );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out UDPSocket_Class;
                          SelfAt : UDPSocket_Class_At ) is
  begin
    InitInstance( Socket_Class(Self), Socket_Class_At(SelfAt), Win32.Winsock.SOCK_DGRAM, Win32.Winsock.IPPROTO_UDP );
  end;

  overriding procedure Finalize( Self : in out UDPSocket_Class ) is
  begin
    Finalize( Socket_Class(Self) );
  end;

  function SetMulticastTtl( Self : in out UDPSocket_Class; Ttl : Integer ) return Boolean is
    OptVal : Win32.ULONG := Win32.ULONG(Ttl);
    OptLen : Win32.INT  := 4;
  begin
    --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
    return Win32.Winsock.setsockopt(ToSocket(Self.SocketId),
                                    Win32.Winsock.IPPROTO_IP,
                                    Win32.Winsock.IP_MULTICAST_TTL,
                                    Win32.To_PCSTR(OptVal'Address),
                                    OptLen) /= Win32.Winsock.SOCKET_ERROR;
  end;

  function SetMulticastInterface( Self : in out UDPSocket_Class; Ifc : String ) return Boolean is
    OptVal : Win32.ULONG := Win32.ULONG(Win32.winsock.inet_addr( Win32.Addr( Ifc )));
    OptLen : Win32.INT  := 4;
  begin
    --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
    return Win32.Winsock.setsockopt(ToSocket(Self.SocketId),
                                    Win32.Winsock.IPPROTO_IP,
                                    Win32.Winsock.IP_MULTICAST_IF,
                                    Win32.To_PCSTR(OptVal'Address),
                                    OptLen) /= Win32.Winsock.SOCKET_ERROR;
  end;

  function AddMulticastMembership( Self : in out UDPSocket_Class; McAddr : String; McIfc : String ) return Boolean is
    OptVal : Win32.Winsock.ip_mreq;
    OptLen : aliased Win32.INT := 8;
    addr : Win32.Winsock.IN_ADDR;
    ifc : Win32.Winsock.IN_ADDR;

    -- When linking with Ws2_32.lib the IP_ADD_MEMBERSHIP should be = 12 instead of 5 (Win32.Winsock.IP_ADD_MEMBERSHIP)
    IP_ADD_MEMBERSHIP : constant := 12;
  begin
    addr.S_Un.s_addr :=  Win32.Winsock.inet_addr(Win32.Addr( McAddr & ASCII.NUL ));
    ifc.S_Un.s_addr := Win32.Winsock.inet_addr(Win32.Addr( McIfc & ASCII.NUL ));
    OptVal.imr_multiaddr := addr;
    OptVal.imr_interface := ifc;
    if Win32.Winsock.setsockopt(Self.SocketId,
                                Win32.Winsock.IPPROTO_IP,
                                IP_ADD_MEMBERSHIP,
                                Win32.To_PCSTR(OptVal'Address),
                                OptLen) = Win32.Winsock.SOCKET_ERROR
    then
      return False;
    end if;
    return True;
  end;

  function DropMulticastMembership( Self : in out UDPSocket_Class; McAddr : String; McIfc : String ) return Boolean is
    OptVal : Win32.Winsock.ip_mreq;
    OptLen : aliased Win32.INT := 8;
    addr : Win32.Winsock.IN_ADDR;
    ifc : Win32.Winsock.IN_ADDR;
  begin
    addr.S_Un.s_addr :=  Win32.Winsock.inet_addr(Win32.Addr( McAddr & ASCII.NUL ));
    ifc.S_Un.s_addr := Win32.Winsock.inet_addr(Win32.Addr( McIfc & ASCII.NUL ));
    OptVal.imr_multiaddr := addr;
    OptVal.imr_interface := ifc;
    if Win32.Winsock.setsockopt(Self.SocketId,
                                Win32.Winsock.IPPROTO_IP,
                                Win32.Winsock.IP_DROP_MEMBERSHIP,
                                Win32.To_PCSTR(OptVal'Address),
                                OptLen) = Win32.Winsock.SOCKET_ERROR
    then
      return False;
    end if;
    return True;
  end;

  --------------------------------------------------------------------------

  function Create return TCPSocket_Class_At is
     Self : TCPSocket_Class_At := null;
  begin
    Self := new TCPSocket_Class;
    InitInstance( Self.all, Self );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out TCPSocket_Class;
                          SelfAt : TCPSocket_Class_At ) is
  begin
    InitInstance( Socket_Class(Self), Socket_Class_At(SelfAt), Win32.Winsock.SOCK_STREAM, Win32.Winsock.IPPROTO_TCP );
  end;

  overriding procedure Finalize( Self : in out TCPSocket_Class ) is
  begin
    Finalize( Socket_Class(Self) );
  end;

  function SetTcpNoDelay( Self : in out TCPSocket_Class; Value : Boolean ) return Boolean is
    OptVal : Win32.ULONG := 0;
    OptLen : Win32.INT  := 4;
  begin
    if Value then
      OptVal := 1;
    end if;
    --function setsockopt(s: TSocket; level, optname: Integer; optval: PAnsiChar; optlen: Integer): Integer; stdcall;
    return Win32.Winsock.setsockopt(ToSocket(Self.SocketId),
                                    Win32.Winsock.IPPROTO_TCP,
                                    Win32.Winsock.TCP_NODELAY,
                                    Win32.To_PCSTR(OptVal'Address),
                                    OptLen) /= Win32.Winsock.SOCKET_ERROR;
  end;

  --------------------------------------------------------------------------

  function Create return TCPClientSocket_Class_At is
     Self : TCPClientSocket_Class_At := null;
  begin
    Self := new TCPClientSocket_Class;
    InitInstance( Self.all, Self );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out TCPClientSocket_Class;
                          SelfAt : TCPClientSocket_Class_At ) is
  begin
    InitInstance( TCPSocket_Class(Self), TCPSocket_Class_At(SelfAt) );
  end;

  overriding procedure Finalize( Self : in out TCPClientSocket_Class ) is
  begin
    Finalize( TCPSocket_Class(Self) );
  end;

  procedure Initialize( Self : in out TCPClientSocket_Class;
                        SocketId : SocketID_T;
                        Connected : Boolean ) is
  begin
    Self.SocketID := SocketId;
    Self.Connected := Connected;
  end;

  overriding function Close( Self : in out TCPClientSocket_Class ) return Boolean is
    dummy : Boolean;
  begin
    dummy := Self.Disconnect;
    return Close( TCPSocket_Class(Self) );
  end;

  function IsConnected( Self : TCPClientSocket_Class ) return Boolean is
  begin
    return Self.Connected;
  end;

  function Connect( Self : in out TCPClientSocket_Class; Ip : String; Port : Integer ) return Boolean is
    addr : aliased Win32.winsock.SOCKADDR := ToAddr(MakeSockAddr(Ip, Port));
    res : Win32.INT := 0;
  begin
    if Self.IsOpen and not Self.Connected then
      res := Win32.Winsock.connect(ToSocket(Self.SocketID), addr'Unchecked_Access, 16);
      Self.Connected := (res /= Win32.Winsock.SOCKET_ERROR);
    end if;
    return Self.Connected;
  end;

  function Disconnect( Self : in out TCPClientSocket_Class ) return Boolean is
    dummy : Boolean;
  begin
    if Self.Connected then
      dummy := Self.Shutdown;
    end if;
    Self.Connected := False;
    return True;
  end;


  --------------------------------------------------------------------------

  function Create return TCPServerSocket_Class_At is
     Self : TCPServerSocket_Class_At := null;
  begin
    Self := new TCPServerSocket_Class;
    InitInstance( Self.all, Self );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out TCPServerSocket_Class;
                          SelfAt : TCPServerSocket_Class_At ) is
  begin
    InitInstance( TCPSocket_Class(Self), TCPSocket_Class_At(SelfAt) );
  end;

  overriding procedure Finalize( Self : in out TCPServerSocket_Class ) is
  begin
    Finalize( TCPSocket_Class(Self) );
  end;

  overriding function Close( Self : in out TCPServerSocket_Class ) return Boolean is
  begin
    Self.Listening := False;
    return Close( TCPSocket_Class(Self) );
  end;

  function Listen( Self : in out TCPServerSocket_Class; MaxBackLog : Integer ) return Boolean is
    res : Win32.INT := 0;
  begin
    if Self.IsOpen and not Self.Listening then
      res := Win32.Winsock.listen(ToSocket(Self.SocketID), Win32.INT(MaxBacklog));
      Self.Listening := (res /= Win32.Winsock.SOCKET_ERROR);
    end if;
    return Self.Listening;
  end;

  function IsListening( Self : TCPServerSocket_Class ) return Boolean is
  begin
    return Self.Listening;
  end;

  function AcceptClient( Self : in out TCPServerSocket_Class; Client : TCPClientSocket_Class_At ) return Boolean is
    Address : aliased Win32.Winsock.SOCKADDR;
    AddressLen : aliased Win32.INT := 16;
    res : Win32.Winsock.SOCKET := 0;
  begin
    res := Win32.Winsock.c_accept(ToSocket(Self.SocketID),
                                  Address'Access,
                                  AddressLen'Access);

    if res /= Win32.Winsock.INVALID_SOCKET then
      client.Initialize(res, true);
      return True;
    end if;
    return False;
  end;


--  /// ------------------------------------------
--  /// Helper to get all IP interfaces
--
--  procedure VVGetIpAddrTable(var p: PMibIpAddrTable; var Size: Cardinal; const bOrder: BOOL);
--  var
--    Res: DWORD;
--  begin
--    p := nil;
--    Size := 0;
--    if @GetIpAddrTable = nil then Exit;   //Not implemented in this windows version
--    Res := GetIpAddrTable(p,Size,bOrder);
--    if Res=ERROR_INSUFFICIENT_BUFFER then begin
--      Getmem(p,Size);
--      // Caller must free this buffer when it is no longer used
--      FillChar(p^,Size,#0);
--      Res := GetIpAddrTable(p,Size,bOrder);
--    end;
--    if Res <> NO_ERROR then begin
--      if Assigned(p) then FreeMem(p);
--      p := nil;
--      Size := 0;
--    end;
--  end;
--
--  function IpAddressToString(Addr: DWORD): AnsiString;
--  var
--    InAddr: TInAddr;
--  begin
--    InAddr.S_addr := Addr;
--    Result := AnsiString(inet_ntoa(InAddr));
--  end;

  package x is new Ada.Text_IO.Modular_IO(Unsigned_32);

-- If argument contains a "/" we assume it is on the form:  subnet-address/subnet-mask
-- e.g "192.168.10.0/255.255.255.0" or "192.168.10.0/24"
-- In that case we loop over all interfaces and take the first one that matches
-- i.e. the one whos interface address is on the subnet
  function doSubnetTranslation(addr : String) return String is
    Idx : Natural;
  begin
    -- If no '/' we just return the given address
    Idx := Ada.Strings.Fixed.Index( addr, "/" );
    if Idx = 0 then
      return addr;
    end if;

    declare
      subnet : String := addr(addr'First .. Idx-1);
      mask   : String := addr(Idx+1 .. addr'Last);
      numBits : Unsigned_32 := 0;
      subnetIp, subnetMask : Unsigned_32;
      dummy : Positive := 1;
--      Size: ULONG;
--      p: PMibIpAddrTable;
--      i: integer;
    begin
      subnetIp := Unsigned_32(Win32.Winsock.inet_addr(Win32.Addr(subnet & ASCII.NUL)));
      if mask'Length <= 2 then
        -- Expand to the number of bits given
        x.Get(mask, numBits, dummy);
        subnetMask := Shift_Left(1, Natural(numBits)) - 1;
        subnetMask := Shift_Left(subnetMask, Natural(32 - numBits));
        subnetMask := Unsigned_32(Win32.Winsock.htonl(Interfaces.C.unsigned_long(subnetMask)));
      else
        subnetMask := Unsigned_32(Win32.Winsock.inet_addr(Win32.Addr(mask & ASCII.NUL)));
      end if;

--TODO    VVGetIpAddrTable(p, Size, False);
--    if Assigned(p) then begin
--      try
--        with p^ do begin
--          for i := 0 to dwNumEntries - 1 do begin
--            with table[i] do begin
--              if (dwAddr and subnetMask) = (subnetIp and subnetMask) then begin
--                Result := IpAddressToString(dwAddr);
--                Break;
--              end;
--            end;
--          end;
--        end;
--      finally
--        FreeMem(p);
--      end;
--    end;
      return subnet;
    end;
  end;


  WSA : Win32.INT;
  wVersionRequired : Win32.WORD := 16#0101#;
  WSAData : aliased Win32.Winsock.WSADATA;
begin
  WSA := Win32.Winsock.WSAStartup( wVersionRequired, WSAData'Unchecked_Access );

end Com_Socket_Pa;

