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

with Ops_Pa.Notifier_Pa;

package Ops_Pa.Error_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type Error_Class    is abstract new Ops_Class with null record;
  type Error_Class_At is access all Error_Class'Class;

  function GetErrorCode( Self : Error_Class ) return Integer is abstract;
  function GetMessage( Self : Error_Class ) return String is abstract;

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  -- Basic implementaion of an error for OPS.
  type BasicError_Class is new Error_Class with private;
  type BasicError_Class_At is access all BasicError_Class'Class;

  ERROR_CODE : constant Integer := 1;

  -- Constructor
  function BasicError(className, method, mess : String) return BasicError_Class_At;

  overriding function GetErrorCode( Self : BasicError_Class ) return Integer;
  overriding function GetMessage( Self : BasicError_Class ) return String;

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  -- Socket error implementaion of an error for OPS.
  type SocketError_Class is new BasicError_Class with private;
  type SocketError_Class_At is access all SocketError_Class'Class;

  function SocketError(ClassName, Method, Mess : String; SocketError : Integer ) return SocketError_Class_At;

  overriding function GetMessage( Self : SocketError_Class ) return String;

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type ErrorService_Class is new Ops_Class with private;
  type ErrorService_Class_At is access all ErrorService_Class'Class;

  function Create return ErrorService_Class_At;

  package ErrorNotifier_Pa is new Notifier_Pa(10, Error_Class_At);

  -- Note that several threads can at the same time report errors so
  -- listeners need to take that into account
  procedure addListener( Self : in out ErrorService_Class; Proc : ErrorNotifier_Pa.OnNotifyEvent_T; Arg : Ops_Class_At );
  procedure removeListener( Self : in out ErrorService_Class; Proc : ErrorNotifier_Pa.OnNotifyEvent_T; Arg : Ops_Class_At );
  procedure addListener( Self : in out ErrorService_Class; Client : ErrorNotifier_Pa.Listener_Interface_At );
  procedure removeListener( Self : in out ErrorService_Class; Client : ErrorNotifier_Pa.Listener_Interface_At );

  -- The ErrorService takes over ownership of error objects from the caller
  -- They will be free'd when all listeners have been notified
  procedure Report( Self : in out ErrorService_Class; Error : Error_Class_At );

  -- Helpers
  procedure Report( Self : in out ErrorService_Class; ClassName, Method, Mess : String);


  -- A static error service that user can connect to.
  -- The static error service is e.g. used for errors during Participant creation.
  function StaticErrorService return ErrorService_Class_At;

private
-- ==========================================================================
--
-- ==========================================================================
  type BasicError_Class is new Error_Class with
    record
      Message : String_At := null;
      ClassName : String_At := null;
      Method : String_At := null;
      ErrorCode : Integer := 0;
    end record;

  procedure InitInstance( Self : in out BasicError_Class; className, method, mess : String );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out BasicError_Class );

-- ==========================================================================
--
-- ==========================================================================
  type SocketError_Class is new BasicError_Class with null record;

  procedure InitInstance( Self : in out SocketError_Class; className, method, mess : String; SocketError : Integer );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out SocketError_Class );

-- ==========================================================================
--
-- ==========================================================================
  type ErrorService_Class is new Ops_Class with
    record
      Notifier : ErrorNotifier_Pa.Notifier_Class_At := null;
    end record;

  procedure InitInstance( Self : in out ErrorService_Class; SelfAt : ErrorService_Class_At );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out ErrorService_Class );

end Ops_Pa.Error_Pa;

