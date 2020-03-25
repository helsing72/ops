--
-- Copyright (C) 2016-2020 Lennart Andersson.
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

with Ops_Pa.Error_Pa;
with Ops_Pa.Notifier_Pa;

use Ops_Pa.Error_Pa;

package Ops_Pa.Transport_Pa is

  type BytesSizePair_T is record
    Bytes : Byte_Arr_At := null;
    Size : Integer := 0;
  end record;

  type ConnectStatus_T is record
    Address : String(1..15) := (others => ' ');
    Port : Integer := 0;
    Connected : Boolean := False;
    TotalNo : Integer := 0;
  end record;

  package ConnectStatusNotifier_Pa is new Ops_Pa.Notifier_Pa(10, ConnectStatus_T, LateArrivals => True);

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type ConnectStatus_Interface is limited interface;
  type ConnectStatus_Interface_At is access all ConnectStatus_Interface'Class;

  procedure OnConnect( Self : in out ConnectStatus_Interface;
                       Sender : in Ops_Class_At;
                       Status : in ConnectStatus_T ) is abstract;

  procedure OnDisconnect( Self : in out ConnectStatus_Interface;
                          Sender : in Ops_Class_At;
                          Status : in ConnectStatus_T ) is abstract;

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type Transport_Class    is abstract new Ops_Class with private;
  type Transport_Class_At is access all Transport_Class'Class;

  --
  procedure SetConnectStatusClient( Self : in out Transport_Class;
                                    Client : ConnectStatus_Interface_At );


  -- Getters/Setters
  function ErrorService( Self : Transport_Class ) return ErrorService_Class_At;
  procedure SetErrorService( Self : in out Transport_Class; es : ErrorService_Class_At );

  function LastErrorCode( Self : Transport_Class ) return Integer;

private
-- ==========================================================================
--
-- ==========================================================================
  type Transport_Class is abstract new Ops_Class with
    record
      CsClient: ConnectStatus_Interface_At := null;

      -- Borrowed reference
      ErrorService : ErrorService_Class_At := null;

      -- Result from WSAGetLastError() on error
      LastErrorCode : Integer := 0;
    end record;

end Ops_Pa.Transport_Pa;
