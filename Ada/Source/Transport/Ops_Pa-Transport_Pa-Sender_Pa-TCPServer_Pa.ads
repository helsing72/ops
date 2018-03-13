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

with Ada.Containers.Vectors;

with Ops_Pa.Socket_Pa,
     Ops_Pa.Signal_Pa,
     Ops_Pa.Mutex_Pa;

package Ops_Pa.Transport_Pa.Sender_Pa.TCPServer_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type TCPServerSender_Class    is new Sender_Class with private;
  type TCPServerSender_Class_At is access all TCPServerSender_Class'Class;

  function Create(serverIP : String;
                  serverPort : Integer;
                  outSocketBufferSize : Int64 := 16000000) return TCPServerSender_Class_At;

  -- Interface used to send data
  overriding function sendTo( Self : in out TCPServerSender_Class; buf : Byte_Arr_At; size : Integer; ip : string; port : Integer) return Boolean;

  overriding function getPort( Self : in out TCPServerSender_Class ) return Integer;
  overriding function getAddress( Self : in out TCPServerSender_Class ) return String;

  overriding procedure Open( Self : in out TCPServerSender_Class );
  overriding procedure Close( Self : in out TCPServerSender_Class );

private
-- ==========================================================================
--
-- ==========================================================================
  function Equal( Left, Right : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At ) return Boolean;

  subtype MyIndex_T is Integer range 0..Integer'Last;
  package MyVector_Pa is new Ada.Containers.Vectors(MyIndex_T, Ops_Pa.Socket_Pa.TCPClientSocket_Class_At, Equal);

-- ==========================================================================
--
-- ==========================================================================
  task type Server_Pr_T( Self : access TCPServerSender_Class'Class ) is
    entry Start;
    entry Finish;
  end Server_Pr_T;

  TerminateEvent_C : constant Ops_Pa.Signal_Pa.Event_T := Ops_Pa.Signal_Pa.Event1_C;
  StartEvent_C     : constant Ops_Pa.Signal_Pa.Event_T := Ops_Pa.Signal_Pa.Event2_C;

-- ==========================================================================
--
-- ==========================================================================
  type TCPServerSender_Class is new Sender_Class with
     record
       Port : Integer := 0;
       IpAddress : String_At := null;
       OutSocketBufferSize : Int64 := -1;

       TcpServer : Ops_Pa.Socket_Pa.TCPServerSocket_Class_At := null;

       ConnectedSockets : MyVector_Pa.Vector;
       ConnectedSocketsMutex : aliased Ops_Pa.Mutex_Pa.Mutex;

       -- Our thread running our Run() method
       Server_Pr : Server_Pr_T(TCPServerSender_Class'Access);
       StopFlag : aliased Boolean := False;
       pragma volatile(StopFlag);
       TerminateFlag : aliased Boolean := False;
       pragma volatile(TerminateFlag);
       EventsToTask : Ops_Pa.Signal_Pa.Signal_T;

       Opened : Boolean := False;
     end record;

  -- Will by called by the thread
  procedure Run( Self : in out TCPServerSender_Class );

  procedure Report( Self : in out TCPServerSender_Class; method : String; mess : String );

  procedure InitInstance( Self : in out TCPServerSender_Class;
                          serverIP : String;
                          serverPort : Integer;
                          outSocketBufferSize : Int64 );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out TCPServerSender_Class );

end Ops_Pa.Transport_Pa.Sender_Pa.TCPServer_Pa;

