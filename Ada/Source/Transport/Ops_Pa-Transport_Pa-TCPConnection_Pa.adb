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

package body Ops_Pa.Transport_Pa.TCPConnection_Pa is

  use type Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;

  procedure Trace(Self : TCPConnection_Class; Msg : String) is
    NameStr : String := "TcpConnection (" & Integer'Image(Self.Port) & ")";
  begin
    Trace(NameStr, Msg);
  end;

  procedure Report( Self : in out TCPConnection_Class; method : string; mess : string ) is
    error : SocketError_Class_At := null;
  begin
    if Self.ErrorService /= null then
      error := SocketError("TCPConnection", method, mess, Self.LastErrorCode);
      Self.ErrorService.Report(Error_Class_At(error));
    end if;
    if TraceEnabled then Self.Trace(method & ": " & mess & ", Error: " & Integer'Image(Self.LastErrorCode)); end if;
  end;

  function Create( Socket : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;
                   Port : Integer
                  ) return TCPConnection_Class_At is
     Self : TCPConnection_Class_At := null;
  begin
    Self := new TCPConnection_Class;
    InitInstance( Self.all, Self, Socket, Port );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out TCPConnection_Class;
                          SelfAt : TCPConnection_Class_At;
                          Socket : Ops_Pa.Socket_Pa.TCPClientSocket_Class_At;
                          Port : Integer ) is
  begin
    Self.Socket := Socket;
    Self.Port := Port;
    Self.DataNotifier := ReceiveNotifier_Pa.Create( Ops_Class_At(SelfAt) );
  end;

  overriding procedure Finalize( Self : in out TCPConnection_Class ) is
    dummy : Boolean;
  begin
    ReceiveNotifier_Pa.Free(Self.DataNotifier);
    if Self.Socket /= null then
      dummy := Self.Socket.Shutdown;
      dummy := Self.Socket.Disconnect;
      Ops_Pa.Socket_Pa.Free(Self.Socket);
    end if;
  end;

  procedure addListener( Self : in out TCPConnection_Class; Client : ReceiveNotifier_Pa.Listener_Interface_At ) is
  begin
    Self.DataNotifier.addListener( Client );
  end;

  procedure removeListener( Self : in out TCPConnection_Class; Client : ReceiveNotifier_Pa.Listener_Interface_At ) is
  begin
    Self.DataNotifier.removeListener( Client );
  end;

  -- SetReceiveBuffer():
  -- Changes the current buffer to use for reads.
  -- Should only be called from the callback.
  procedure SetReceiveBuffer( Self : in out TCPConnection_Class; bytes : Byte_Arr_At; size : Integer) is
  begin
    Self.Buffer := bytes;
    if bytes /= null then
      -- check that buffer is large enough, 23 bytes minimum
      if size < 23 then
        raise Buffer_Too_Small;
      end if;
    end if;
    Self.BufferSize := size;
  end;

  subtype ByteArr4_T is Byte_Arr(0..3);
  function ToByteArr is new Ada.Unchecked_Conversion(Integer, ByteArr4_T);
  function ToByte    is new Ada.Unchecked_Conversion(Character, Byte);

  opsp_tcp_size_info_header : Byte_Arr(0..17) :=
    (ToByte('o'),
     ToByte('p'),
     ToByte('s'),
     ToByte('p'),
     ToByte('_'),
     ToByte('t'),
     ToByte('c'),
     ToByte('p'),
     ToByte('_'),
     ToByte('s'),
     ToByte('i'),
     ToByte('z'),
     ToByte('e'),
     ToByte('_'),
     ToByte('i'),
     ToByte('n'),
     ToByte('f'),
     ToByte('o')
    );

  procedure SendData( Self : in out TCPConnection_Class; buf : Byte_Arr_At; size : Integer; errorFlag : in out boolean ) is
    sizeInfo : Byte_Arr(0..21);
  begin
    -- First, prepare a package of fixed length 22 with information about the size of the data package
    sizeinfo(0 ..17) := opsp_tcp_size_info_header;
    sizeinfo(18..21) := ToByteArr(size);

    if TraceEnabled then Self.Trace("SendData, length= " & Integer'Image(size) & ")"); end if;

    -- Send prepared size info
    if Self.Socket.SendBuf(sizeInfo) = 22 then
      -- Send the actual data
      if Self.Socket.SendBuf(buf.all(buf'first..buf'first+Byte_Arr_Index_T(size)-1)) /= size then
        Self.LastErrorCode := Self.Socket.GetLatestError;
        errorFlag := True;
      end if;
    else
      Self.LastErrorCode := Self.Socket.GetLatestError;
      errorFlag := True;
    end if;
  end;

  -- Exit Run as fast as possible
  procedure Stop( Self : in out TCPConnection_Class ) is
  begin
    Self.StopFlag := True;
  end;

  -- Performs the reading and notification of data to the client
  -- Called when the socket is connected, exits on error or stop
  procedure Run( Self : in out TCPConnection_Class ) is
    SizeInfoSize_C : constant Byte_Arr_Index_T := 22;
    type TPhase is (phSize, phPayload);

    Res : Integer := 0;
    BufferIdx : Byte_Arr_Index_T := 0;
    BufferIdxLast : Byte_Arr_Index_T := 0;
    Phase : TPhase := phSize;
    ErrorDetected : Boolean := False;

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
        Self.LastErrorCode := Ops_Pa.Socket_Pa.SOCKET_ERROR_C;
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
      Res := Integer(BufferIdxLast - Self.Buffer'First + 1);
      if TraceEnabled then Self.Trace("Notify(data packet received, length= " & Integer'Image(Res) & ")"); end if;

      -- Notify upper layer with a data packet
      Self.DataNotifier.DoNotify(BytesSizePair_T'(Self.Buffer, Res));

      -- Set up for reading a new size info
      SetupForReadingSize;
    end;

  begin
    Self.StopFlag := False;
    Self.LastErrorCode := 0;

    -- Set up for reading a new size info
    SetupForReadingSize;

    -- Read data loop
    while (not Self.StopFlag) and (Self.Socket.IsConnected) loop
      if TraceEnabled then Self.Trace("Wait for Data..."); end if;
      Res := Self.Socket.ReceiveBuf( Self.Buffer(BufferIdx..BufferIdxLast) );
      if TraceEnabled then Self.Trace("Got some Data"); end if;

      exit when Self.StopFlag;

      if Res = 0 then
        -- Connection closed gracefully
        if TraceEnabled then Self.Trace("Connection closed gracefully"); end if;
        exit;

      elsif Res < 0 then
        -- Some error
        Self.LastErrorCode := Self.Socket.GetLatestError;
        Self.Report("Run", "Read failed");
        --          notifyNewEvent(BytesSizePair(NULL, -2));
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
            exit;
          end if;
        else
          -- Continue to read bytes
          null;
        end if;
      end if;
    end loop;
  end;

end Ops_Pa.Transport_Pa.TCPConnection_Pa;

