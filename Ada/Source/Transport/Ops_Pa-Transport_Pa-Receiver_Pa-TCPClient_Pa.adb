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

package body Ops_Pa.Transport_Pa.Receiver_Pa.TCPClient_Pa is

  use type Com_Socket_Pa.TCPClientSocket_Class_At;

  function Create( serverIP : string;
                   serverPort : Integer;
                   inSocketBufferSize : Int64 := 16000000) return TCPClientReceiver_Class_At is
     Self : TCPClientReceiver_Class_At := null;
  begin
    Self := new TCPClientReceiver_Class;
    InitInstance( Self.all, Self, serverIP, serverPort, inSocketBufferSize );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out TCPClientReceiver_Class;
                          SelfAt : TCPClientReceiver_Class_At;
                          serverIP : String;
                          serverPort : Integer;
                          inSocketBufferSize : Int64 ) is
  begin
    InitInstance( Receiver_Class(Self), Receiver_Class_At(SelfAt) );

    Self.IpAddress := Copy(serverIP);
    Self.Port := serverPort;
    Self.InSocketBufferSize := inSocketBufferSize;

    -- Create socket
    Self.TcpClient := Com_Socket_Pa.Create;
  end;

  overriding procedure Finalize( Self : in out TCPClientReceiver_Class ) is
  begin
    Stop( Self );   -- Make sure socket is closed

    Finalize( Receiver_Class(Self) );  -- Make sure thread is terminated

    if Self.TcpClient /= null then
      Com_Socket_Pa.Free(Self.TcpClient);
    end if;

    if Self.IpAddress /= null then
      Dispose(Self.IpAddress);
    end if;
  end;

  procedure Report( Self : in out TCPClientReceiver_Class; method : string; mess : string ) is
    error : SocketError_Class_At := null;
  begin
    if Self.ErrorService /= null then
      error := SocketError("TCPClientReceiver", method, mess, Self.LastErrorCode);
      Self.ErrorService.Report(Error_Class_At(error));
    end if;
  end;

  -- Start():
  -- Starts the receiver, and reads bytes into given buffer.
  -- When a message is read, a callback (notification) will be done with the
  -- buffer and actual number of bytes read.
  -- When the callback returns a new read is started to the current buffer
  overriding function Start( Self: in out TCPClientReceiver_Class; bytes : Byte_Arr_At; size : Integer) return Boolean is
  begin
    Self.LastErrorCode := 0;
    if Self.TcpClient.IsOpen then
      return True;
    end if;

    if not Self.TcpClient.Open then
      Self.LastErrorCode := Self.TcpClient.GetLatestError;
      Report(Self, "Start", "Socket could not be created");
      return False;
    end if;

    Self.StopFlag := False;
    Self.Buffer := bytes;
    Self.BufferSize := size;

    if Self.Buffer /= null then
      -- Start a thread running our run() method
      Self.EventsToTask.Signal(StartEvent_C);
    end if;
    return True;
  end;

  -- Override from Receiver
  -- Used to get the sender IP and port for a received message
  -- Only safe to call in callback
  overriding function GetSourceIP( Self : in out TCPClientReceiver_Class ) return String is
  begin
    return Self.IpAddress.all;
  end;

  overriding function GetSourcePort( Self : in out TCPClientReceiver_Class ) return Integer is
  begin
    return Self.Port;
  end;

  -- SetReceiveBuffer():
  -- Changes the current buffer to use for reads.
  -- Should only be called from the callback.
  overriding procedure SetReceiveBuffer( Self : in out TCPClientReceiver_Class; bytes : Byte_Arr_At; size : Integer) is
  begin
    Self.Buffer := bytes;
    Self.BufferSize := size;
  end;

  -- Stop():
  -- Aborts an ongoing read. NOTE: Must NOT be called from the callback.
  overriding procedure Stop( Self : in out TCPClientReceiver_Class ) is
    dummy : Boolean;
  begin
    if Self.TcpClient.IsOpen then
      -- Tell run to exit
      Self.StopFlag := True;

      -- Thread is probably waiting in a read, so we must close the socket
      dummy := Self.TcpClient.Shutdown;
      dummy := Self.TcpClient.Disconnect;

      if not Self.TcpClient.Close then
        Self.LastErrorCode := Self.TcpClient.GetLatestError;
      end if;

      Self.Buffer := null;
      Self.BufferSize := 0;
    end if;
  end;

  overriding function Port( Self : TCPClientReceiver_Class ) return Integer is
    Port : Integer := 0;
  begin
    if Self.TcpClient.GetBoundPort( Port ) then
      return Port;
    else
      return 0;
    end if;
  end;

  overriding function Address( Self : TCPClientReceiver_Class ) return String is
  begin
    return Self.TcpClient.GetBoundIP;
  end;

  overriding procedure Run( Self : in out TCPClientReceiver_Class ) is
    SizeInfoSize_C : constant Byte_Arr_Index_T := 22;
    type TPhase is (phSize, phPayload);

    Res : Integer := 0;
    BufferIdx : Byte_Arr_Index_T := 0;
    BufferIdxLast : Byte_Arr_Index_T := 0;
    Phase : TPhase := phSize;
    ErrorDetected : Boolean := False;
    dummy : Boolean;

    procedure SetupForReadingSize is
    begin
      Phase := phSize;
      BufferIdx := Self.Buffer'First;
      BufferIdxLast := BufferIdx + SizeInfoSize_C - 1;
    end;

    procedure HandleSizeInfo is
      BytesToRead : Byte_Arr_Index_T;
    begin
      -- Get size of data packet from the received size packet
      BytesToRead :=
        16#0000_0001# * Byte_Arr_Index_T(Self.Buffer(Self.Buffer'First+18)) +
        16#0000_0100# * Byte_Arr_Index_T(Self.Buffer(Self.Buffer'First+19)) +
        16#0001_0000# * Byte_Arr_Index_T(Self.Buffer(Self.Buffer'First+20)) +
        16#0100_0000# * Byte_Arr_Index_T(Self.Buffer(Self.Buffer'First+21));

      if BytesToRead > Byte_Arr_Index_T(Self.BufferSize) then
        -- This is an error, we are not able to receive more than the buffer size
        Self.LastErrorCode := Com_Socket_Pa.SOCKET_ERROR_C;
        Self.Report("HandleSizeInfo", "Error in read size info");
        --notifyNewEvent(BytesSizePair(NULL, -1));
        ErrorDetected := True;
      end if;

      Phase := phPayload;
      BufferIdx := Self.Buffer'First;
      BufferIdxLast := BufferIdx + BytesToRead - 1;
    end;

    procedure HandlePayload is
    begin
      -- Notify upper layer with a data packet
      Self.DataNotifier.DoNotify(BytesSizePair_T'(Self.Buffer, Res));

      -- Set up for reading a new size info
      SetupForReadingSize;
    end;

    procedure OpenAndConnect is
    begin
      dummy := Self.TcpClient.Open;
      dummy := Self.TcpClient.Connect( Self.IpAddress.all, Self.Port );
    end;

    procedure DisconnectAndClose is
    begin
      dummy := Self.TcpClient.Disconnect;
      dummy := Self.TcpClient.Close;
    end;

  begin
    while not Self.StopFlag loop
      begin
        -- Connect loop
        while (not Self.StopFlag) and (not Self.TcpClient.IsConnected) loop
          DisconnectAndClose;
          delay 0.100;
          OpenAndConnect;
        end loop;
        exit when Self.StopFlag;

        dummy := Self.TcpClient.SetNonBlocking(False);

        -- Connected, Set buffer size
        if Self.InSocketBufferSize > 0 then
          dummy := Self.TcpClient.SetReceiveBufferSize(Integer(Self.InSocketBufferSize));
          if Self.TcpClient.GetReceiveBufferSize /= Integer(Self.InSocketBufferSize) then
            Self.LastErrorCode := Com_Socket_Pa.SOCKET_ERROR_C;
            Self.Report("Run", "Socket buffer size could not be set");
          end if;
        end if;

        -- Disable Nagle algorithm
        dummy := Self.TcpClient.SetTcpNoDelay(True);

--      notifyNewEvent(BytesSizePair(NULL, -5)); //Connection was down but has been reastablished.

        -- Set up for reading a new size info
        SetupForReadingSize;

        ErrorDetected := False;

        -- Read data loop
        while (not Self.StopFlag) and (Self.TcpClient.IsConnected) loop
          Res := Self.TcpClient.ReceiveBuf( Self.Buffer(BufferIdx..BufferIdxLast) );
          exit when Self.StopFlag;
          if Res = 0 then
            -- Connection closed gracefully
            DisconnectAndClose;
            exit;

          elsif Res < 0 then
            -- Some error
            Self.LastErrorCode := Self.TcpClient.GetLatestError;
            Self.Report("Run", "Read failed");
            --          notifyNewEvent(BytesSizePair(NULL, -2));
            DisconnectAndClose;
            exit;

          else
            -- Read OK
            BufferIdx := BufferIdx + Byte_Arr_Index_T(Res);
            if BufferIdx > BufferIdxLast then
              -- Expected number of bytes read
              case Phase is
                when phSize => HandleSizeInfo;
                when phPayload => HandlePayload;
              end case;
              if ErrorDetected then
                DisconnectAndClose;
                exit;
              end if;
            else
              -- Continue to read bytes
              null;
            end if;
          end if;
        end loop;
        DisconnectAndClose;
      exception
        when others =>
          Self.LastErrorCode := Com_Socket_Pa.SOCKET_ERROR_C;
          Self.Report("Run", "Exception ");
          DisconnectAndClose;
      end;
    end loop;
  end;

end Ops_Pa.Transport_Pa.Receiver_Pa.TCPClient_Pa;

