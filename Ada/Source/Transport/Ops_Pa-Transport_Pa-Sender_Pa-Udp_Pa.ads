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

with Win32.Winsock;

package Ops_Pa.Transport_Pa.Sender_Pa.Udp_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type UdpSender_Class    is new Sender_Class with private;
  type UdpSender_Class_At is access all UdpSender_Class'Class;

  -- Constructs a new UDPSender and binds its underlying socket to local host
  -- and a dynamically allocated local port.
  -- This class accepts synchronous write operations through sendTo().
  function Create(localInterface : String := "0.0.0.0";
                  ttl : Integer := 1;
                  outSocketBufferSize : Int64 := 16000000;
                  multicastSocket : Boolean := False) return UdpSender_Class_At;

  -- Interface used to send data
  overriding function sendTo( Self : in out UdpSender_Class; buf : Byte_Arr_At; size : Integer; ip : string; port : Integer) return Boolean;

  overriding function getPort( Self : in out UdpSender_Class ) return Integer;
  overriding function getAddress( Self : in out UdpSender_Class ) return String;

  overriding procedure Open( Self : in out UdpSender_Class );
  overriding procedure Close( Self : in out UdpSender_Class );

private
  type UdpSender_Class is new Sender_Class with
    record
      LocalInterface : String_At := null;
      Ttl : Integer := 1;
      OutSocketBufferSize : Int64 := -1;
      MulticastSocket : Boolean := True;

      SocketId : Win32.Winsock.SOCKET := Win32.Winsock.INVALID_SOCKET;
    end record;

  procedure Report( Self : in out UdpSender_Class; method : string; mess : string);

  procedure InitInstance( Self : in out UdpSender_Class;
                          localInterface : String;
                          ttl : Integer;
                          outSocketBufferSize : Int64;
                          multicastSocket : Boolean);

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out UdpSender_Class );

end Ops_Pa.Transport_Pa.Sender_Pa.Udp_Pa;

