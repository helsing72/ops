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

with Ada.Unchecked_Conversion;

with Ops_Pa.Socket_Pa,
     Ops_Pa.Signal_Pa;

package body Ops_Pa.Transport_Pa.Sender_Pa.TCPServer_Pa is

  use type Ada.Containers.Count_Type;
  use type Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;
  use type TCPConnection_Pa.TCPConnection_Class_At;
  use type Ops_Pa.Signal_Pa.Event_T;

  function Equal( Left, Right : TCPConnection_Pa.TCPConnection_Class_At ) return Boolean is
  begin
    return Left = Right;
  end;

  function Create(serverIP : String;
                  serverPort : Integer;
                  outSocketBufferSize : Int64 := 16000000) return TCPServerSender_Class_At is
    Self : TCPServerSender_Class_At := null;
  begin
    Self := new TCPServerSender_Class;
    InitInstance( Self.all, serverIP, serverPort, outSocketBufferSize );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out TCPServerSender_Class;
                          serverIP : String;
                          serverPort : Integer;
                          outSocketBufferSize : Int64) is
  begin
    Self.Port := serverPort;
    Self.IpAddress := Copy(serverIP);
    Self.OutSocketBufferSize := outSocketBufferSize;

    Self.TcpServer := Ops_Pa.Socket_Pa.Create;

    Self.Server_Pr.Start;
  end;

  overriding procedure Finalize( Self : in out TCPServerSender_Class ) is
  begin
    Close( Self );

    Self.TerminateFlag := True;
    Self.EventsToTask.Signal(TerminateEvent_C);
    Self.Server_Pr.Finish;

    Ops_Pa.Socket_Pa.Free(Self.TcpServer);

    if Self.IpAddress /= null then
      Dispose( Self.IpAddress );
    end if;
  end;

  procedure Report( Self : in out TCPServerSender_Class; method : string; mess : string ) is
    error : SocketError_Class_At := null;
  begin
    if Self.ErrorService /= null then
      error := SocketError("TCPServerSender", method, mess, Self.LastErrorCode);
      Self.ErrorService.Report(Error_Class_At(error));
    end if;
  end;

  overriding procedure Open( Self : in out TCPServerSender_Class ) is
  begin
    if not Self.Opened then
      Self.Opened := True;
      Self.StopFlag := False;

      -- Start a thread running our run() method
      Self.EventsToTask.Signal(StartEvent_C);
    end if;
  end;

  overriding procedure Close( Self : in out TCPServerSender_Class ) is
    dummy : Boolean;
  begin
    if Self.Opened then
      Self.Opened := False;

      -- Tell thread to terminate
      Self.StopFlag := True;

      dummy := Self.TcpServer.Close;

      -- Free all connected sockets, which implicitly will close all connections
      declare
        S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.ConnectedSocketsMutex'Access);
      begin
        for i in Self.ConnectedSockets.First_Index .. Self.ConnectedSockets.Last_Index loop
          if Self.ConnectedSockets.Element(i) /= null then
            TCPConnection_Pa.Free(Self.ConnectedSockets.Element(i));
          end if;
        end loop;
      end;
    end if;
  end;

  overriding function getAddress( Self : in out TCPServerSender_Class ) return String is
  begin
    return Self.IpAddress.all;
  end;

  overriding function getPort( Self : in out TCPServerSender_Class ) return Integer is
  begin
    return Self.Port;
  end;

  -- Sends buf to any Receiver connected to this Sender, ip and port are discarded and can be left blank.
  overriding function sendTo( Self : in out TCPServerSender_Class; buf : Byte_Arr_At; size : Integer; ip : string; port : Integer) return Boolean is
    errorFlag : Boolean;
    S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.ConnectedSocketsMutex'Access);
  begin
    -- Send to anyone connected. Loop backwards to avoid problems when removing broken sockets
    for i in reverse Self.ConnectedSockets.First_Index .. Self.ConnectedSockets.Last_Index loop
      ErrorFlag := False;
      begin
        if Self.ConnectedSockets.Element(i) /= null then
          Self.ConnectedSockets.Element(i).SendData(buf, size, ErrorFlag);
        end if;
      exception
        when others =>
          ErrorFlag := True;
      end;

      if ErrorFlag then
        Self.LastErrorCode := Self.ConnectedSockets.Element(i).LastErrorCode;
        Report(Self, "sendTo", "Error sending");
        TCPConnection_Pa.Free( Self.ConnectedSockets.Element(i) );
        Self.ConnectedSockets.Delete(i);
      end if;
    end loop;
    return True;
  end;

  task body Server_Pr_T is
    Events : Ops_Pa.Signal_Pa.Event_T;
  begin
    accept Start;
    while not Self.TerminateFlag loop
      begin
        Self.EventsToTask.WaitForAny(Events);
        exit when (Events and TerminateEvent_C) /= Ops_Pa.Signal_Pa.NoEvent_C;
        if (Events and StartEvent_C) /= Ops_Pa.Signal_Pa.NoEvent_C then
          Self.Run;
        end if;
      exception
        when others =>
          Self.ErrorService.Report( "TCPServer", "Server_Pr", "Got exception from Run()" );
      end;
    end loop;
    accept Finish;
  end Server_Pr_T;

  procedure Run( Self : in out TCPServerSender_Class ) is
    tcpClient : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At := null;
    dummy : Boolean;
  begin
    while not Self.StopFlag loop
      begin
        -- Setup server socket for listening
        if not Self.TcpServer.Open then
          Self.LastErrorCode := Self.TcpServer.GetLatestError;
          Report(Self, "Run", "Socket could not be created");
        end if;
        if not Self.TcpServer.SetReuseAddress(True) then
          Report(Self, "Run", "Failed to set REUSE_ADDRESS for server socket");
        end if;
        if not Self.TcpServer.Bind( Self.IpAddress.all, Self.Port ) then
          Self.LastErrorCode := Self.TcpServer.GetLatestError;
          Report(Self, "Run", "Socket could not be bound");
        end if;
        if not Self.TcpServer.Listen( 10 ) then
          Self.LastErrorCode := Self.TcpServer.GetLatestError;
          Report(Self, "Run", "Socket Listen failed");
        end if;

        if not Self.TcpServer.IsListening then
          Self.LastErrorCode := Self.TcpServer.GetLatestError;

        else
          -- Keep listening for connecting clients
          while not Self.StopFlag loop
            -- Create a client socket ready for the calling client
            tcpClient := Ops_Pa.Socket_Pa.Create;

            -- accept()
            while not Self.StopFlag and not Self.TcpServer.AcceptClient(tcpClient) loop
              delay 0.010;
            end loop;
            exit when Self.StopFlag;

            -- Now we have a connected client, setup some parameters
            if Self.OutSocketBufferSize > 0 then
              dummy:= tcpClient.SetSendBufferSize(Integer(Self.OutSocketBufferSize));
              if tcpClient.GetSendBufferSize /= Integer(Self.OutSocketBufferSize) then
                Self.LastErrorCode := Ops_Pa.Socket_Pa.SOCKET_ERROR_C;
                Report(Self, "Run", "Socket buffer size could not be set");
              end if;
            end if;

            -- Disable Nagle algorithm
            dummy := tcpClient.SetTcpNoDelay(True);

            -- and put it in list and then wait for another connection
            declare
              S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.ConnectedSocketsMutex'Access);
              Port : Integer := 0;
              Conn : TCPConnection_Pa.TCPConnection_Class_At := null;
            begin
              if tcpClient.GetBoundPort(Port) then null; end if;
              Conn := TCPConnection_Pa.Create( tcpClient, Port );
              Self.ConnectedSockets.Append( Conn );
            end;
            tcpClient := null;   -- Clear ref since list now owns object
          end loop;
        end if;
      exception
        when others =>
          null;
      end;

      dummy := Self.TcpServer.Close;
      delay 0.100;

      if tcpClient /= null then
        Ops_Pa.Socket_Pa.Free(tcpClient);
        tcpClient := null;
      end if;
    end loop;
  end;

end Ops_Pa.Transport_Pa.Sender_Pa.TCPServer_Pa;

