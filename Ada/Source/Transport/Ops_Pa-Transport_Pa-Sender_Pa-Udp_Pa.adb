--
-- Copyright (C) 2016-2018 Lennart Andersson.
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

package body Ops_Pa.Transport_Pa.Sender_Pa.Udp_Pa is

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
    Self.UdpSocket := Ops_Pa.Socket_Pa.Create;
    Open( Self );
  end;

  overriding procedure Finalize( Self : in out UdpSender_Class ) is
  begin
    Close( Self );

    Ops_Pa.Socket_Pa.Free(Self.UdpSocket);

    if Self.LocalInterface /= null then
      Dispose(Self.LocalInterface);
    end if;
  end;

  procedure Report( Self : in out UdpSender_Class; method : string; mess : string ) is
    error : SocketError_Class_At := null;
  begin
    if Self.ErrorService /= null then
      error := SocketError("UDPSender", method, mess, Self.LastErrorCode);
      Self.ErrorService.Report(Error_Class_At(error));
    end if;
  end;

  overriding procedure Open( Self : in out UdpSender_Class ) is
  begin
    Self.LastErrorCode := 0;
    if Self.UdpSocket.IsOpen then
      return;
    end if;

    if not Self.UdpSocket.Open then
      Self.LastErrorCode := Self.UdpSocket.GetLatestError;
      Report(Self, "Open", "Socket could not be created");
      return;
    end if;

    if not Self.UdpSocket.SetNonBlocking( True ) then
      Self.LastErrorCode := Self.UdpSocket.GetLatestError;
    end if;

    -- Set socket buffer size
    if Self.OutSocketBufferSize > 0 then
      if not Self.UdpSocket.SetSendBufferSize( Integer(Self.OutSocketBufferSize) ) then
        Self.LastErrorCode := Self.UdpSocket.GetLatestError;
      end if;

      if Self.UdpSocket.GetSendBufferSize /= Integer(Self.OutSocketBufferSize) then
        if Self.LastErrorCode = 0 then
          Self.LastErrorCode := Ops_Pa.Socket_Pa.SOCKET_ERROR_C;
        end if;
        Report(Self, "Start", "Socket buffer size could not be set");
      end if;
    end if;

    if Self.MulticastSocket then
      if not Self.UdpSocket.SetMulticastTtl( Self.Ttl ) then
        Self.LastErrorCode := Self.UdpSocket.GetLatestError;
      end if;

      if not Self.UdpSocket.SetMulticastInterface( Self.LocalInterface.all ) then
        Self.LastErrorCode := Self.UdpSocket.GetLatestError;
      end if;
    end if;

    if not Self.UdpSocket.Bind( "0.0.0.0", 0 ) then
      Self.LastErrorCode := Self.UdpSocket.GetLatestError;
    end if;
  end;

  overriding procedure Close( Self : in out UdpSender_Class ) is
    dummy : Boolean;
  begin
    Self.LastErrorCode := 0;
    if Self.UdpSocket.IsOpen then
      dummy := Self.UdpSocket.Shutdown;

      if not Self.UdpSocket.Close then
        Self.LastErrorCode := Self.UdpSocket.GetLatestError;
      end if;
    end if;
  end;

  overriding function getAddress( Self : in out UdpSender_Class ) return String is
    res : String := Self.UdpSocket.GetBoundIP;
  begin
    if res = "" then
      Self.LastErrorCode := Self.UdpSocket.GetLatestError;
      Report(Self, "getAddress", "Failed to get bound address");
    end if;
    return res;
  end;

  overriding function getPort( Self : in out UdpSender_Class ) return Integer is
    Port : Integer := 0;
  begin
    if not Self.UdpSocket.GetBoundPort( Port ) then
      Self.LastErrorCode := Self.UdpSocket.GetLatestError;
      Report(Self, "getPort", "Failed to get bound port");
    end if;
    return Port;
  end;

  overriding function sendTo( Self : in out UdpSender_Class; buf : Byte_Arr_At; size : Integer; ip : string; port : Integer) return Boolean is
  begin
    Self.LastErrorCode := 0;
    if not Self.UdpSocket.IsOpen then
      return False;
    end if;

    if Self.UdpSocket.SendTo( buf.all(buf'First..buf'First+Byte_Arr_Index_T(Size)-1), Ip, Port ) /= Size then
      Self.LastErrorCode := Self.UdpSocket.GetLatestError;
      Report(Self, "sendTo", "sendto() failed. IP: " & ip & ", port: " & Integer'Image(port));
      return False;
    end if;

    return True;
  end;

end Ops_Pa.Transport_Pa.Sender_Pa.Udp_Pa;

