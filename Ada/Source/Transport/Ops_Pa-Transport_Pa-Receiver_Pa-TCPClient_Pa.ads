--
-- Copyright (C) 2016-2019 Lennart Andersson.
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

with Ops_Pa.Socket_Pa;
with Ops_Pa.Transport_Pa.TCPConnection_Pa;
with Ops_Pa.DeadlineNotifier_Pa;

package Ops_Pa.Transport_Pa.Receiver_Pa.TCPClient_Pa is

  package Timer_Pa is new DeadlineNotifier_Pa(10);

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type TCPClientReceiver_Class is new Receiver_Class and
    TCPConnection_Pa.ReceiveNotifier_Pa.Listener_Interface and
    Timer_Pa.DeadlineListener_Interface with
      private;
  type TCPClientReceiver_Class_At is access all TCPClientReceiver_Class'Class;

  function Create( serverIP : string;
                   serverPort : Integer;
                   HeartbeatPeriod : Int32;
                   HeartbeatTimeout : Int32;
                   inSocketBufferSize : Int64 := 16000000) return TCPClientReceiver_Class_At;

  -- Start():
  -- Starts the receiver, and reads bytes into given buffer.
  -- When a message is read, a callback (notification) will be done with the
  -- buffer and actual number of bytes read.
  -- When the callback returns a new read is started to the current buffer
  overriding function Start( Self: in out TCPClientReceiver_Class; bytes : Byte_Arr_At; size : Integer) return Boolean;

  -- GetSource():
  -- Used to get the sender IP and port for a received message.
  -- Should only be called from the callback.
  overriding function GetSourceIP( Self : in out TCPClientReceiver_Class ) return String;
  overriding function GetSourcePort( Self : in out TCPClientReceiver_Class ) return Integer;

  -- SetReceiveBuffer():
  -- Changes the current buffer to use for reads.
  -- Should only be called from the callback.
  overriding procedure SetReceiveBuffer( Self : in out TCPClientReceiver_Class; bytes : Byte_Arr_At; size : Integer);

  -- Stop():
  -- Aborts an ongoing read. NOTE: Must NOT be called from the callback.
  overriding procedure Stop( Self : in out TCPClientReceiver_Class );

  overriding function Port( Self : TCPClientReceiver_Class ) return Integer;
  overriding function Address( Self : TCPClientReceiver_Class ) return String;

  overriding procedure SetErrorService( Self : in out TCPClientReceiver_Class; es : ErrorService_Class_At );

  -- Set this flag to enable trace from the TCP Client
  TraceEnabled : Boolean := False;

private
-- ==========================================================================
--
-- ==========================================================================
  type TCPClientReceiver_Class is new Receiver_Class and
    TCPConnection_Pa.ReceiveNotifier_Pa.Listener_Interface and
    Timer_Pa.DeadlineListener_Interface with
    record
      SelfAt : TCPClientReceiver_Class_At := null;
      Port : Integer := 0;
      IpAddress : String_At := null;
      InSocketBufferSize : Int64 := 0;

      TcpClient : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At := null;
      Connection : Ops_Pa.Transport_Pa.TCPConnection_Pa.TCPConnection_Class_At := null;

      Timer : Timer_Pa.DeadlineNotifier_Class_At := null;
    end record;

  overriding procedure Run( Self : in out TCPClientReceiver_Class );

  procedure Report( Self : in out TCPClientReceiver_Class; method : string; mess : string);

  -- Called whenever the receiver has new data.
  procedure OnNotify( Self : in out TCPClientReceiver_Class; Sender : in Ops_Class_At; Item : in BytesSizePair_T );

  overriding procedure OnDeadlineMissed( Self : in out TCPClientReceiver_Class; Sender : in Ops_Class_At );

  procedure InitInstance( Self : in out TCPClientReceiver_Class;
                          SelfAt : TCPClientReceiver_Class_At;
                          serverIP : string;
                          serverPort : Integer;
                          HeartbeatPeriod : Int32;
                          HeartbeatTimeout : Int32;
                          inSocketBufferSize : Int64 );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out TCPClientReceiver_Class );

end Ops_Pa.Transport_Pa.Receiver_Pa.TCPClient_Pa;

