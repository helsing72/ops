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
with Ops_Pa.Notifier_Pa;

package Ops_Pa.Transport_Pa.TCPConnection_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type TCPConnection_Class    is new Transport_Class with private;
  type TCPConnection_Class_At is access all TCPConnection_Class'Class;

  -- Takes ownership of socket handle
  function Create( Socket : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;
                   Port : Integer;
                   HeartbeatPeriod : Int32;
                   HeartbeatTimeout : Int32 ) return TCPConnection_Class_At;

  package ReceiveNotifier_Pa is new Notifier_Pa(1, BytesSizePair_T);

  procedure addListener( Self : in out TCPConnection_Class; Client : ReceiveNotifier_Pa.Listener_Interface_At );
  procedure removeListener( Self : in out TCPConnection_Class; Client : ReceiveNotifier_Pa.Listener_Interface_At );

  -- SetReceiveBuffer():
  -- Changes the current buffer to use for reads.
  -- Should only be called at init time before Run() is called and after that from the callback.
  procedure SetReceiveBuffer( Self : in out TCPConnection_Class; bytes : Byte_Arr_At; size : Integer );

  Buffer_Too_Small : exception;

  -- Sends given data. Sets errorFlag to true if an error occur, otherwise unchanged
  -- size == 0, trigs a periodic check
  procedure SendData( Self : in out TCPConnection_Class; buf : Byte_Arr_At; size : Integer; errorFlag : in out boolean );

  -- Used by TCPClient at connect
  procedure SendProbe( Self : in out TCPConnection_Class; errorFlag : in out boolean );

  procedure PeriodicCheck( Self : in out TCPConnection_Class; errorFlag : in out boolean );

  -- Performs the reading and notification of data to the client.
  -- SetReceiveBuffer() must be called before Run() is called
  -- Called when the socket is connected, exits on error or stop
  -- NOTE it is the callers responsibility that Run() has exited before Free() is called
  procedure Run( Self : in out TCPConnection_Class );

  -- Performs the reading and notification of data to the client
  -- SetReceiveBuffer() must be called before Run() is called
  -- Called in polling mode when there is data for the socket, exits when data handled
  procedure Poll( Self : in out TCPConnection_Class; ErrorDetected : in out Boolean );

  -- Tell class to exit Run() as fast as possible
  procedure Stop( Self : in out TCPConnection_Class );

  -- Return ref to underlaying socket
  function GetSocket( Self : TCPConnection_Class ) return Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;

  function GetUserData( Self : TCPConnection_Class ) return Integer;
  procedure SetUserData( Self : in out TCPConnection_Class; Value : Integer );

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type TCPServerConnection_Class    is new TCPConnection_Class with private;
  type TCPServerConnection_Class_At is access all TCPServerConnection_Class'Class;

  -- Takes ownership of socket handle
  function Create( Socket : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;
                   Port : Integer;
                   HeartbeatPeriod : Int32;
                   HeartbeatTimeout : Int32 ) return TCPServerConnection_Class_At;

  -- Set this flag to enable trace from the TCP Connection
  TraceEnabled : Boolean := False;

private
-- ==========================================================================
--
-- ==========================================================================
  type TPhase is (phSize, phPayload);

  type TCPConnection_Class is new Transport_Class with
    record
      Socket : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At := null;
      Port : Integer := 0;

      -- Current read buffer from user
      Buffer : Byte_Arr_At := null;
      BufferSize : Integer := 0;
      TimeRcv : TimeMs_T := 0;

      BufferIdx : Byte_Arr_Index_T := 0;
      BufferIdxLast : Byte_Arr_Index_T := 0;
      Phase : TPhase := phSize;

      StopFlag : aliased Boolean := False;
      pragma volatile(StopFlag);

      -- Used for notifications to users of the Receiver
      DataNotifier : ReceiveNotifier_Pa.Notifier_Class_At := null;

      -- Buffer for the message header
      SizeBuffer : Byte_Arr_At := null;

      -- Buffer for the probe/heartbeat message
      ProbeBuffer : Byte_Arr_At := null;
      DetectedVersion : Integer := 1;

      -- Send time
      TimeSnd : TimeMs_T := 0;
      HbSent : Boolean := False;
      HeartbeatPeriod : TimeMs_T := 1000;
      HeartbeatTimeout : TimeMs_T := 3000;

      UserData : Integer := 0;
    end record;

  procedure InitInstance( Self : in out TCPConnection_Class;
                          SelfAt : TCPConnection_Class_At;
                          Socket : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;
                          Port : Integer;
                          HeartbeatPeriod : Int32;
                          HeartbeatTimeout : Int32
                         );

  procedure SetupForReadingSize( Self : in out TCPConnection_Class );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out TCPConnection_Class );

-- ==========================================================================
--
-- ==========================================================================
  type TCPServerConnection_Class is new TCPConnection_Class with
    record
      ServerBuffer : Byte_Arr_At := null;
    end record;

  procedure InitInstance( Self : in out TCPServerConnection_Class;
                          SelfAt : TCPServerConnection_Class_At;
                          Socket : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;
                          Port : Integer;
                          HeartbeatPeriod : Int32;
                          HeartbeatTimeout : Int32
                         );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out TCPServerConnection_Class );

end Ops_Pa.Transport_Pa.TCPConnection_Pa;

