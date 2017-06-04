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

with Ops_Pa.Error_Pa;
use Ops_Pa.Error_Pa;

package Ops_Pa.Transport_Pa.Sender_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type Sender_Class    is abstract new Ops_Class with private;
  type Sender_Class_At is access all Sender_Class'Class;

  -- Interface used to send data
  function sendTo( Self : in out Sender_Class; buf : Byte_Arr_At; size : Integer; ip : string; port : Integer) return Boolean is abstract;

  function getPort( Self : in out Sender_Class ) return Integer is abstract;
  function getAddress( Self : in out Sender_Class ) return String is abstract;

  procedure Open( Self : in out Sender_Class ) is abstract;
  procedure Close( Self : in out Sender_Class ) is abstract;

  -- Getters/Setters
  function ErrorService( Self : Sender_Class ) return ErrorService_Class_At;
  procedure SetErrorService( Self : in out Sender_Class; es : ErrorService_Class_At );

  function LastErrorCode( Self : Sender_Class ) return Integer;

  -- Sender Factory (creates a sender)
  function createMCSender(localInterface : string := "0.0.0.0"; ttl : Integer := 1; outSocketBufferSize : Int64 := 16000000) return Sender_Class_At;
  function createUDPSender(localInterface : string := "0.0.0.0"; ttl : Integer := 1; outSocketBufferSize : Int64 := 16000000) return Sender_Class_At;
  function createTCPServer(ip : string; port : Integer; outSocketBufferSize : Int64 := 16000000) return Sender_Class_At;

private
-- ==========================================================================
--
-- ==========================================================================
  type Sender_Class is abstract new Ops_Class with
    record
      -- Borrowed reference
      ErrorService : ErrorService_Class_At := null;

      -- Result from socket lib on error
      LastErrorCode : Integer := 0;
    end record;

end Ops_Pa.Transport_Pa.Sender_Pa;

