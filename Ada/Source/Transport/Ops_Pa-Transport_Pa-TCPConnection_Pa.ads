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
                   Port : Integer ) return TCPConnection_Class_At;

  package ReceiveNotifier_Pa is new Notifier_Pa(1, BytesSizePair_T);

  procedure addListener( Self : in out TCPConnection_Class; Client : ReceiveNotifier_Pa.Listener_Interface_At );
  procedure removeListener( Self : in out TCPConnection_Class; Client : ReceiveNotifier_Pa.Listener_Interface_At );

  -- SetReceiveBuffer():
  -- Changes the current buffer to use for reads.
  -- Should only be called at init time before Run() is called and after that from the callback.
  procedure SetReceiveBuffer( Self : in out TCPConnection_Class; bytes : Byte_Arr_At; size : Integer );

  Buffer_Too_Small : exception;

  -- Sends given data. Sets errorFlag to true if an error occur, otherwise unchanged
  procedure SendData( Self : in out TCPConnection_Class; buf : Byte_Arr_At; size : Integer; errorFlag : in out boolean );

  -- Performs the reading and notification of data to the client
  -- Called when the socket is connected, exits on error or stop
  procedure Run( Self : in out TCPConnection_Class );

  -- Exit Run as fast as possible
  procedure Stop( Self : in out TCPConnection_Class );

  -- Set this flag to enable trace from the TCP Connection
  TraceEnabled : Boolean := False;

private
-- ==========================================================================
--
-- ==========================================================================
  type TCPConnection_Class is new Transport_Class with
    record
      Socket : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At := null;
      Port : Integer := 0;

      -- Current read buffer from user
      Buffer : Byte_Arr_At := null;
      BufferSize : Integer := 0;

      StopFlag : aliased Boolean := False;
      pragma volatile(StopFlag);

      -- Used for notifications to users of the Receiver
      DataNotifier : ReceiveNotifier_Pa.Notifier_Class_At := null;
    end record;

  procedure InitInstance( Self : in out TCPConnection_Class;
                          SelfAt : TCPConnection_Class_At;
                          Socket : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;
                          Port : Integer
                         );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out TCPConnection_Class );

end Ops_Pa.Transport_Pa.TCPConnection_Pa;

