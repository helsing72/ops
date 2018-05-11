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

with Ops_Pa.OpsObject_Pa.Domain_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.Transport_Pa.Receiver_Pa,
     Ops_Pa.SerializableFactory_Pa.CompFactory_Pa,
     Ops_Pa.Error_Pa,
     Ops_Pa.OpsObject_Pa.OPSMessage_Pa,
     Ops_Pa.MemoryMap_Pa,
     Ops_Pa.ByteBuffer_Pa,
     Ops_Pa.ArchiverInOut_Pa,
     Ops_Pa.ArchiverInOut_Pa.ArchiverIn_Pa,
     Ops_Pa.Notifier_Pa,
     Ops_Pa.Mutex_Pa;

use  Ops_Pa.Transport_Pa.Receiver_Pa;

package Ops_Pa.Transport_Pa.ReceiveDataHandler_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type ReceiveDataHandler_Class is new Ops_Class and
    ReceiveNotifier_Pa.Listener_Interface with
      private;
  type ReceiveDataHandler_Class_At is access all ReceiveDataHandler_Class'Class;

  function Create(top : Ops_Pa.OpsObject_Pa.Topic_Pa.Topic_Class_At;
                  dom : Ops_Pa.OpsObject_Pa.Domain_Pa.Domain_Class_At;
                  opsObjectFactory : Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.SerializableInheritingTypeFactory_Class_At;
                  Reporter : Ops_Pa.Error_Pa.ErrorService_Class_At) return ReceiveDataHandler_Class_At;

  procedure acquireMessageLock( Self : in out ReceiveDataHandler_Class );
  procedure releaseMessageLock( Self : in out ReceiveDataHandler_Class );

  procedure Stop( Self : in out ReceiveDataHandler_Class );

  -- Set the default capacity to handle many subscribers (may happen when many topics use the same transport and port).
  package MessageNotifier_Pa is new Ops_Pa.Notifier_Pa(100, Ops_Pa.OpsObject_Pa.OPSMessage_Pa.OPSMessage_Class_At);

  procedure addListener( Self : in out ReceiveDataHandler_Class; Client : MessageNotifier_Pa.Listener_Interface_At );
  procedure removeListener( Self : in out ReceiveDataHandler_Class; Client : MessageNotifier_Pa.Listener_Interface_At );

  function getReceiver( Self : ReceiveDataHandler_Class ) return Ops_Pa.Transport_Pa.Receiver_Pa.Receiver_Class_At;

  function getSampleMaxSize( Self : ReceiveDataHandler_Class ) return Int32;
  function getNumListeners( Self : ReceiveDataHandler_Class ) return Int32;

private
-- ==========================================================================
--
-- ==========================================================================
  type ReceiveDataHandler_Class is new Ops_Class and
    ReceiveNotifier_Pa.Listener_Interface with
    record
      SelfAt : ReceiveDataHandler_Class_At := null;

      -- Borrowed references
      ErrorService : Ops_Pa.Error_Pa.ErrorService_Class_At := null;

      -- Used for notifications to users of the ReceiveDataHandler
      DataNotifier : MessageNotifier_Pa.Notifier_Class_At := null;

      -- The receiver used for this ReceiveHandler.
      Receiver : Ops_Pa.Transport_Pa.Receiver_Pa.Receiver_Class_At := null;

      -- Preallocated MemoryMap and buffer for receiving data
      MemMap : Ops_Pa.MemoryMap_Pa.MemoryMap_Class_At := null;
      Buffer : Ops_Pa.ByteBuffer_Pa.ByteBuffer_Class_At := null;
      SampleMaxSize : Int32 := 0;

      -- Archiver used for deserializing byte buffers into messages
      Archiver : Ops_Pa.ArchiverInOut_Pa.ArchiverIn_Pa.ArchiverIn_Class_At := null;

      -- Temporary MemoryMap and buffer used during basic validation of a segment
      TmpMemMap : Ops_Pa.MemoryMap_Pa.MemoryMap_Class_At := null;
      TmpBuffer : Ops_Pa.ByteBuffer_Pa.ByteBuffer_Class_At := null;

      -- Current OPSMessage, valid until next sample arrives.
      Message : Ops_Pa.OpsObject_Pa.OPSMessage_Pa.OPSMessage_Class_At := null;

      -- The accumulated size in bytes of the current message being received
      CurrentMessageSize : Integer := 0;

      --
      MessageLock : aliased Ops_Pa.Mutex_Pa.Mutex;

      -- Status variables for the reception
      ExpectedSegment : UInt32 := 0;
      FirstReceived : Boolean := False;
    end record;

  -- Called whenever the receiver has new data.
  procedure OnNotify( Self : in out ReceiveDataHandler_Class; Sender : in Ops_Class_At; Item : in BytesSizePair_T );

  -- Handles spare bytes, i.e. extra bytes in buffer not consumed by created message
  procedure calculateAndSetSpareBytes( Self : in out ReceiveDataHandler_Class; mess : Ops_Pa.OpsObject_Pa.OPSMessage_Pa.OPSMessage_Class_At; segmentPaddingSize : Integer);

  procedure InitInstance( Self : in out ReceiveDataHandler_Class;
                          SelfAt : ReceiveDataHandler_Class_At;
                          top : Ops_Pa.OpsObject_Pa.Topic_Pa.Topic_Class_At;
                          dom : Ops_Pa.OpsObject_Pa.Domain_Pa.Domain_Class_At;
                          opsObjectFactory : Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.SerializableInheritingTypeFactory_Class_At;
                          Reporter : Ops_Pa.Error_Pa.ErrorService_Class_At );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out ReceiveDataHandler_Class );

end Ops_Pa.Transport_Pa.ReceiveDataHandler_Pa;

