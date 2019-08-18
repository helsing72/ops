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

package body Ops_Pa.Error_Pa is

  -- Constructor
  function BasicError(className, method, mess : String) return BasicError_Class_At is
    Self : BasicError_Class_At := null;
  begin
    Self := new BasicError_Class;
    InitInstance( Self.all, className, method, mess );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  overriding function GetErrorCode( Self : BasicError_Class ) return Integer is
  begin
    return Self.ErrorCode;
  end;

  overriding function GetMessage( Self : BasicError_Class ) return String is
  begin
    return Self.ClassName.all & "." & Self.Method.all & "(): " & Self.Message.all;
  end;

  procedure InitInstance( Self : in out BasicError_Class; className, method, mess : String ) is
  begin
    Self.Message := Copy(mess);
    Self.ClassName := Copy(className);
    Self.Method := Copy(method);
    Self.ErrorCode := ERROR_CODE;
  end;

  overriding procedure Finalize( Self : in out BasicError_Class ) is
  begin
    if Self.Message /= null then
      Dispose(Self.Message);
    end if;
    if Self.ClassName /= null then
      Dispose(Self.ClassName);
    end if;
    if Self.Method /= null then
      Dispose(Self.Method);
    end if;
  end;


  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function SocketError(ClassName, Method, Mess : String; SocketError : Integer) return SocketError_Class_At is
    Self : SocketError_Class_At := null;
  begin
    Self := new SocketError_Class;
    InitInstance( Self.all, className, method, mess, SocketError );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  overriding function GetMessage( Self : SocketError_Class ) return String is
    res : String := GetMessage( BasicError_Class(Self) );
  begin
    if Self.ErrorCode /= -1 then
      return res & " [error code:" & Integer'Image(Self.ErrorCode) & "]";
    end if;
    return res;
  end;

  procedure InitInstance( Self : in out SocketError_Class; className, method, mess : String; SocketError : Integer ) is
  begin
    InitInstance ( BasicError_Class(Self), className, method, mess );
    Self.ErrorCode := SocketError;
  end;

  overriding procedure Finalize( Self : in out SocketError_Class ) is
  begin
    Finalize( BasicError_Class(Self) );
  end;


  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------

  function Create return ErrorService_Class_At is
    Self : ErrorService_Class_At := null;
  begin
    Self := new ErrorService_Class;
    InitInstance( Self.all, Self );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure addListener( Self : in out ErrorService_Class; Proc : ErrorNotifier_Pa.OnNotifyEvent_T; Arg : Ops_Class_At ) is
  begin
    Self.Notifier.addListener( Proc, Arg );
  end;

  procedure removeListener( Self : in out ErrorService_Class; Proc : ErrorNotifier_Pa.OnNotifyEvent_T; Arg : Ops_Class_At ) is
  begin
    Self.Notifier.removeListener( Proc, Arg );
  end;

  procedure addListener( Self : in out ErrorService_Class; Client : ErrorNotifier_Pa.Listener_Interface_At ) is
  begin
    Self.Notifier.addListener( Client );
  end;

  procedure removeListener( Self : in out ErrorService_Class; Client : ErrorNotifier_Pa.Listener_Interface_At ) is
  begin
    Self.Notifier.removeListener( Client );
  end;

  -- The ErrorService takes over ownership of error objects from the caller
  -- They will be free'd when all listeners have been notified
  procedure Report( Self : in out ErrorService_Class; Error : Error_Class_At ) is
  begin
    begin
      Self.Notifier.doNotify(Error);
    exception
      when others =>
        null;
    end;
    Free(Error);
  end;

    -- Helpers
  procedure Report( Self : in out ErrorService_Class; ClassName, Method, Mess : String) is
    error : BasicError_Class_At := BasicError( ClassName, Method, Mess );
  begin
    Self.Report( Error_Class_At(error) );
  end;

  procedure InitInstance( Self : in out ErrorService_Class; SelfAt : ErrorService_Class_At ) is
  begin
    Self.Notifier := ErrorNotifier_Pa.Create( Ops_Class_At(SelfAt) );
  end;

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out ErrorService_Class ) is
  begin
    ErrorNotifier_Pa.Free(Self.Notifier);
  end;

  gStaticErrorService : ErrorService_Class_At := Create;

  -- A static error service that user can connect to.
  -- The static error service is e.g. used for errors during Participant creation.
  function StaticErrorService return ErrorService_Class_At is
  begin
    return gStaticErrorService;
  end;

  procedure Debug_TotalClear is
  begin
    if gStaticErrorService /= null then
      Free(gStaticErrorService);
      gStaticErrorService := null;
    end if;
  end;

end Ops_Pa.Error_Pa;

