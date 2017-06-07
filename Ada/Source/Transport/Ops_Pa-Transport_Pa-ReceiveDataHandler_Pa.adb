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

with Ops_Pa.ByteBuffer_Pa,
     Ops_Pa.MemoryMap_Pa,
     Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa,
     Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
     Ops_Pa.ArchiverInOut_Pa,
     Ops_Pa.ArchiverInOut_Pa.ArchiverIn_Pa;

use Ops_Pa.ByteBuffer_Pa,
    Ops_Pa.MemoryMap_Pa,
    Ops_Pa.Error_Pa,
    Ops_Pa.OpsObject_Pa,
    Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
    Ops_Pa.ArchiverInOut_Pa,
    Ops_Pa.ArchiverInOut_Pa.ArchiverIn_Pa;

package body Ops_Pa.Transport_Pa.ReceiveDataHandler_Pa is

  function Create(top : Ops_Pa.OpsObject_Pa.Topic_Pa.Topic_Class_At;
                  dom : Ops_Pa.OpsObject_Pa.Domain_Pa.Domain_Class_At;
                  opsObjectFactory : Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.SerializableInheritingTypeFactory_Class_At;
                  Reporter : Ops_Pa.Error_Pa.ErrorService_Class_At) return ReceiveDataHandler_Class_At is
    Self : ReceiveDataHandler_Class_At := null;
  begin
    Self := new ReceiveDataHandler_Class;
    InitInstance( Self.all, Self, top, dom, opsObjectFactory, Reporter );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out ReceiveDataHandler_Class;
                          SelfAt : ReceiveDataHandler_Class_At;
                          top : Ops_Pa.OpsObject_Pa.Topic_Pa.Topic_Class_At;
                          dom : Ops_Pa.OpsObject_Pa.Domain_Pa.Domain_Class_At;
                          opsObjectFactory : Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.SerializableInheritingTypeFactory_Class_At;
                          Reporter : Ops_Pa.Error_Pa.ErrorService_Class_At ) is
  begin
    Self.SelfAt := SelfAt;
    Self.ErrorService := Reporter;

    Self.DataNotifier := MessageNotifier_Pa.Create( Ops_Class_At(SelfAt) );

    Self.MemMap := Ops_Pa.MemoryMap_Pa.Create(UInt32((top.SampleMaxSize / PACKET_MAX_SIZE) + 1), PACKET_MAX_SIZE);
    Self.Buffer := Ops_Pa.ByteBuffer_Pa.Create(Self.MemMap);
    Self.SampleMaxSize := top.SampleMaxSize;

    Self.Archiver := Ops_Pa.ArchiverInOut_Pa.ArchiverIn_Pa.Create(buf => Self.Buffer, fact => opsObjectFactory);

    -- Temporary MemoryMap and buffer
    Self.TmpMemMap := Ops_Pa.MemoryMap_Pa.Create(Segment => null, Size => 0);
    Self.TmpBuffer := Ops_Pa.ByteBuffer_Pa.Create(mMap => Self.TmpMemMap);

    Self.Receiver := Ops_Pa.Transport_Pa.Receiver_Pa.getReceiver(top, dom, Reporter => Self.ErrorService);
    if Self.Receiver = null then
      raise ECommException;
    end if;

    Ops_Pa.Transport_Pa.Receiver_Pa.addListener(Self.Receiver.all, ReceiveNotifier_Pa.Listener_Interface_At(SelfAt));
  end;

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out ReceiveDataHandler_Class ) is
  begin
    Stop( Self );
    Free(Self.Receiver);
    Free(Self.TmpBuffer);
    Free(Self.TmpMemMap);
    Free(Self.Archiver);
    Free(Self.Buffer);
    Free(Self.MemMap);
    MessageNotifier_Pa.Free(Self.DataNotifier);
  end;

  procedure acquireMessageLock( Self : in out ReceiveDataHandler_Class ) is
  begin
    Self.MessageLock.Acquire;
  end;

  procedure releaseMessageLock( Self : in out ReceiveDataHandler_Class ) is
  begin
    Self.MessageLock.Release;
  end;

  procedure addListener( Self : in out ReceiveDataHandler_Class; Client : MessageNotifier_Pa.Listener_Interface_At ) is
    res : Boolean;
    S : Com_Mutex_Pa.Scope_Lock(Self.MessageLock'Access);
  begin
    Self.DataNotifier.addListener(Client);
    if Self.DataNotifier.numListeners = 1 then
      Self.ExpectedSegment := 0;
      Self.CurrentMessageSize := 0;
      res := Self.Receiver.Start(Self.MemMap.GetSegment(Self.ExpectedSegment), Integer(Self.MemMap.SegmentSize));
    end if;
  end;

  procedure removeListener( Self : in out ReceiveDataHandler_Class; Client : MessageNotifier_Pa.Listener_Interface_At ) is
    S : Com_Mutex_Pa.Scope_Lock(Self.MessageLock'Access);
  begin
    -- By taking the lock (above), we also make sure that the receiveDataHandler::onNewEvent() can't be in
    -- the callback of a listener
    Self.DataNotifier.removeListener(Client);
    if Self.DataNotifier.numListeners = 0 then
      Self.Receiver.Stop;
    end if;
  end;

  function getReceiver( Self : ReceiveDataHandler_Class ) return Ops_Pa.Transport_Pa.Receiver_Pa.Receiver_Class_At is
  begin
    return Self.Receiver;
  end;

  function getSampleMaxSize( Self : ReceiveDataHandler_Class ) return Int32 is
  begin
    return Self.SampleMaxSize;
  end;

  function getNumListeners( Self : ReceiveDataHandler_Class ) return Int32 is
  begin
    return Int32(Self.DataNotifier.numListeners);
  end;

  -- Called whenever the receiver has new data.
  procedure OnNotify( Self : in out ReceiveDataHandler_Class; Sender : in Ops_Class_At; Item : in BytesSizePair_T ) is

    procedure Report(msg : String) is
    begin
      if Self.ErrorService /= null then
        Self.ErrorService.Report( "ReceiveDataHandler", "onNewEvent", msg );
      end if;
    end;

    procedure Report(msg : string; addr : string; port : Integer) is
    begin
      Report(msg & " [" & addr & "::" & Integer'Image(port) & "]");
    end;

    nrOfFragments : Int32;
    currentFragment : Int32;
    segmentPaddingSize : Int32;
    oldMessage, newMessage : Ops_Pa.OpsObject_Pa.OPSMessage_Pa.OPSMessage_Class_At;
    srcAddr : String := Self.Receiver.GetSourceIP;
    srcPort : Integer := self.Receiver.GetSourcePort;
    Result : Boolean;
  begin
    if Item.size <= 0 then
--            //Inform participant that we had an error waiting for data,
--            //this means the underlying socket is down but hopefully it will reconnect, so no need to do anything.
--            //Only happens with tcp connections so far.
--
--            if (byteSizePair.size == -5)
--            {
--                Report('Connection was lost but is now reconnected.');
--            }
--            else
--            {
--                Report('Empty message or error.');
--            }

      -- Continue with the same buffer, so just exit
      return;
    end if;

    -- TODO Check that all segments come from the same source (IP and port)

    -- Use a temporary map and buf to peek data before putting it in to FMemMap
    Self.TmpMemMap.ChangeBuffer(Self.MemMap.getSegment(Self.ExpectedSegment), Self.MemMap.SegmentSize);
    Self.TmpBuffer.Reset;   -- Reset buffer to use changed memmap values

    -- Check protocol
    Self.TmpBuffer.CheckProtocol( Result );
    if Result then
      --Read of message ID and fragmentation info, this is ignored so far.
      --std::string messageID = tBuf.ReadString();
      Self.TmpBuffer.ReadInt( nrOfFragments );
      Self.TmpBuffer.ReadInt( currentFragment );

      if (currentFragment /= (nrOfFragments - 1)) and (Item.size /= PACKET_MAX_SIZE) then
        Report("Debug: Received broken package.", srcAddr, srcPort);
      end if;

      Self.CurrentMessageSize := Self.CurrentMessageSize + Item.size;

      if currentFragment /= Int32(Self.ExpectedSegment) then
        -- Error
        if Self.FirstReceived then
          Report("Segment Error, sample will be lost.", srcAddr, srcPort);
          Self.FirstReceived := False;
        end if;
        Self.ExpectedSegment := 0;
        Self.CurrentMessageSize := 0;
        Self.Receiver.SetReceiveBuffer(Self.MemMap.getSegment(Self.ExpectedSegment), Integer(Self.MemMap.SegmentSize));
        return;
      end if;

      if currentFragment = (nrOfFragments - 1) then
        -- We have got all segments
        Self.FirstReceived := True;
        self.ExpectedSegment := 0;
        Self.Buffer.Reset;

        -- Skip some protocol parts already checked
        declare
          dummy : Int32;
        begin
          Self.Buffer.CheckProtocol( Result );
          Self.Buffer.ReadInt( dummy );
          Self.Buffer.ReadInt( dummy );
        end;

        segmentPaddingSize := Int32(Self.Buffer.GetSize);

        -- Read of the actual OPSMessage
        newMessage := null;
        begin
          newMessage := OPSMessage_Class_At(Self.Archiver.inout2("message", Serializable_Class_At(newMessage)));
          if newMessage /= null then
            -- Check that we succeded in creating the actual data message
            if newMessage.Data /= null then
              -- Put spare bytes in data-field of message
              Self.calculateAndSetSpareBytes(newMessage, Integer(segmentPaddingSize));

              -- Add IP and port for source as meta data into OPSMessage
              newMessage.setSource(srcAddr, srcPort);
            else
              Report("Failed to deserialize message. Check added Factories.", srcAddr, srcPort);
              Free(newMessage);
              newMessage := null;
            end if;
          else
            -- Inform participant that invalid data is on the network.
            Report("Unexpected type received. Type creation failed.", srcAddr, srcPort);
          end if;
        exception
          when E : others =>
            Free(newMessage);
            newMessage := null;
            Report("Invalid data on network. Exception ", srcAddr, srcPort);
        end;

        Self.CurrentMessageSize := 0;

        if newMessage /= null then
          declare
            S : Com_Mutex_Pa.Scope_Lock(Self.MessageLock'Access);
          begin
            oldMessage := Self.Message;
            Self.Message := newMessage;
            newMessage := null;

            -- Increment ref count for message
            Self.Message.Reserve;

            -- Send it to Subscribers
            Self.DataNotifier.doNotify(Self.Message);

            -- This will delete the old message if no one has reserved it in the application layer.
            if oldMessage /= null then
              oldMessage.Unreserve;
            end if;
          end;
        end if;

      else
        Self.ExpectedSegment := Self.ExpectedSegment + 1;

        if Self.ExpectedSegment >= Self.MemMap.NrOfSegments then
          Report("Buffer too small for received message.", srcAddr, srcPort);
          Self.ExpectedSegment := 0;
          Self.CurrentMessageSize := 0;
        end if;
      end if;
      Self.Receiver.SetReceiveBuffer(Self.MemMap.getSegment(Self.ExpectedSegment), Integer(Self.MemMap.SegmentSize));
    else
      --Inform participant that invalid data is on the network.
      Report("Protocol ERROR.", srcAddr, srcPort);
      Self.Receiver.SetReceiveBuffer(Self.MemMap.getSegment(Self.ExpectedSegment), Integer(Self.MemMap.SegmentSize));
    end if;
  end;

  procedure Stop( Self : in out ReceiveDataHandler_Class ) is
  begin
    Ops_Pa.Transport_Pa.Receiver_Pa.removeListener(Self.Receiver.all, ReceiveNotifier_Pa.Listener_Interface_At(Self.SelfAt));

    -- Need to release the last message we received, if any.
    -- (We always keep a reference to the last message received)
    if Self.Message /= null then
      Self.Message.Unreserve;
    end if;

    Self.Message := null;
    --/TODO  receiver->stop(); ???

  end;

  -- Handles spare bytes, i.e. extra bytes in buffer not consumed by created message
  procedure calculateAndSetSpareBytes(Self : in out ReceiveDataHandler_Class; mess : Ops_Pa.OpsObject_Pa.OPSMessage_Pa.OPSMessage_Class_At; segmentPaddingSize : Integer) is
    nrOfSerializedBytes : Integer;
    totalNrOfSegments : Integer;
    nrOfSerializedSegements : Integer;
    nrOfUnserializedSegments : Integer;
    nrOfSpareBytes : Integer;
  begin
    -- We must calculate how many unserialized segment headers we have and substract
    -- that total header size from the size of spareBytes.
    nrOfSerializedBytes := Integer(Self.Buffer.GetSize);
    totalNrOfSegments := Self.CurrentMessageSize / Integer(Self.MemMap.SegmentSize);
    nrOfSerializedSegements := nrOfSerializedBytes / Integer(Self.MemMap.SegmentSize);
    nrOfUnserializedSegments := totalNrOfSegments - nrOfSerializedSegements;

    nrOfSpareBytes := Self.CurrentMessageSize - Integer(Self.Buffer.GetSize) - (nrOfUnserializedSegments * segmentPaddingSize);

    if nrOfSpareBytes > 0 then
      declare
        arr : Byte_Arr_At := new Byte_Arr(1..Byte_Arr_Index_T(nrOfSpareBytes));
      begin
        -- This will read the rest of the bytes as raw bytes and put them into spareBytes field of data.
        Self.Buffer.ReadChars( arr.all );
        mess.SetSpareBytes(arr);
      end;
    end if;
  end;

end Ops_Pa.Transport_Pa.ReceiveDataHandler_Pa;

