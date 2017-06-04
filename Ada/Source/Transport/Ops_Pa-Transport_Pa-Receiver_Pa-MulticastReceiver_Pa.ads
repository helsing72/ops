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

with Com_Socket_Pa;

package Ops_Pa.Transport_Pa.Receiver_Pa.MulticastReceiver_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type MulticastReceiver_Class    is new Receiver_Class with private;
  type MulticastReceiver_Class_At is access all MulticastReceiver_Class'Class;

  function Create( mcAddress : string;
                   bindPort : Integer;
                   localInterface : string := "0.0.0.0";
                   inSocketBufferSize : Int64 := 16000000) return MulticastReceiver_Class_At;

  -- Start():
  -- Starts the receiver, and reads bytes into given buffer.
  -- When a message is read, a callback (notification) will be done with the
  -- buffer and actual number of bytes read.
  -- When the callback returns a new read is started to the current buffer
  overriding function Start( Self: in out MulticastReceiver_Class; bytes : Byte_Arr_At; size : Integer) return Boolean;

  -- GetSource():
  -- Used to get the sender IP and port for a received message.
  -- Should only be called from the callback.
  overriding function GetSourceIP( Self : in out MulticastReceiver_Class ) return String;
  overriding function GetSourcePort( Self : in out MulticastReceiver_Class ) return Integer;

  -- SetReceiveBuffer():
  -- Changes the current buffer to use for reads.
  -- Should only be called from the callback.
  overriding procedure SetReceiveBuffer( Self : in out MulticastReceiver_Class; bytes : Byte_Arr_At; size : Integer);

  -- Stop():
  -- Aborts an ongoing read. NOTE: Must NOT be called from the callback.
  overriding procedure Stop( Self : in out MulticastReceiver_Class );

  function available( Self : MulticastReceiver_Class ) return Boolean;

  overriding function Port( Self : MulticastReceiver_Class ) return Integer;
  overriding function Address( Self : MulticastReceiver_Class ) return String;

private
-- ==========================================================================
--
-- ==========================================================================
  type MulticastReceiver_Class is new Receiver_Class with
     record
       Port : Integer := 0;
       IpAddress : String_At := null;
       LocalInterface : String_At := null;
       InSocketBufferSize : Int64 := 0;

       UdpSocket : Com_Socket_Pa.UDPSocket_Class_At := null;

       -- Current read buffer from user
       Buffer : Byte_Arr_At := null;
       BufferSize : Integer := 0;
     end record;

  function ReceiveMessage( Self : in out MulticastReceiver_Class; o: Byte_Arr_At; size: Integer ) return Integer;

  overriding procedure Run( Self : in out MulticastReceiver_Class );

  procedure Report( Self : in out MulticastReceiver_Class; method : string; mess : string);

  procedure InitInstance( Self : in out MulticastReceiver_Class;
                          SelfAt : MulticastReceiver_Class_At;
                          mcAddress : string;
                          bindPort : Integer;
                          localInterface : string;
                          inSocketBufferSize : Int64 );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out MulticastReceiver_Class );

end Ops_Pa.Transport_Pa.Receiver_Pa.MulticastReceiver_Pa;

