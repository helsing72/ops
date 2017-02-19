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

with Com_SyncPrimitives_Pa,
     Ops_Pa.Error_Pa;
use Com_SyncPrimitives_Pa,
    Ops_Pa.Error_Pa;

package body Ops_Pa.OpsObject_Pa.OPSMessage_Pa is

  -- Constructors
  function Create return OPSMessage_Class_At is
    Self : OPSMessage_Class_At := null;
  begin
    Self := new OPSMessage_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  procedure Serialize( Self : in out OPSMessage_Class; archiver : ArchiverInOut_Class_At) is
  begin
    Serialize( OpsObject_Class(Self), archiver );
    archiver.Inout("messageType", Self.messageType);
    archiver.Inout("publisherPriority", Self.publisherPriority);
    archiver.Inout("publicationID", Self.publicationID);
    archiver.Inout("publisherName", Self.publisherName);
    archiver.Inout("topicName", Self.topicName);
    archiver.Inout("topLevelKey", Self.topLevelKey);
    archiver.Inout("address", Self.address);
    Self.data := Ops_Pa.OpsObject_Pa.OPSObject_Class_At(archiver.Inout2("data", Serializable_Class_At(Self.data)));
  end;

  procedure Reserve( Self : in out OPSMessage_Class ) is
    tmp : Ctv.Integer32;
  begin
    tmp := InterlockedIncrement(Self.NrOfReservations'Access);
  end;

  procedure UnReserve( Self : in out OPSMessage_Class ) is
  begin
    if InterlockedDecrement(Self.NrOfReservations'Access) = 0 then
      Free(Self'Access);
    end if;
  end;

  function NrOfReservations( Self : OPSMessage_Class ) return Ctv.Integer32 is
  begin
    return Self.NrOfReservations;
  end;

  procedure setSource( Self : in out OPSMessage_Class; addr : string; port : Integer) is
  begin
    Replace(Self.SourceIP, addr);
    Self.SourcePort := port;
  end;

  procedure getSource( Self : OPSMessage_Class; addr : in out string; port : in out Integer) is
  begin
    addr := Self.SourceIP.all;
    port := Self.SourcePort;
  end;

  function DataOwner( Self : OPSMessage_Class ) return Boolean is
  begin
    return Self.DataOwner;
  end;

  procedure SetDataOwner( Self : in out OPSMessage_Class; value : Boolean ) is
  begin
    Self.DataOwner := value;
  end;

  function PublicationID( Self : OPSMessage_Class ) return Int64 is
  begin
    return Self.publicationID;
  end;

  procedure SetPublicationID( Self : in out OPSMessage_Class; value : Int64 ) is
  begin
    Self.publicationID := value;
  end;

  function PublisherName( Self : OPSMessage_Class ) return String is
  begin
    return Self.publisherName.all;
  end;

  procedure SetPublisherName( Self : in out OPSMessage_Class; value : String ) is
  begin
    Replace(Self.publisherName, value);
  end;

  function TopicName( Self : OPSMessage_Class ) return String is
  begin
    return Self.topicName.all;
  end;

  procedure SetTopicName( Self : in out OPSMessage_Class; value : String ) is
  begin
    Replace(Self.topicName, value);
  end;

  function Data( Self : OPSMessage_Class ) return OPSObject_Class_At is
  begin
    return Self.data;
  end;

  procedure SetData( Self : in out OPSMessage_Class; value : OPSObject_Class_At ) is
  begin
    if Self.DataOwner and Self.data /= null then
      Free(Self.data);
    end if;
    Self.data := value;
  end;

  -- Returns a newely allocated deep copy/clone of Self.
  function Clone( Self : OPSMessage_Class ) return OpsObject_Class_At is
    Result : OPSMessage_Class_At := null;
  begin
    Result := Create;
    Self.FillClone( OpsObject_Class_At(Result) );
    return OpsObject_Class_At(Result);
  end Clone;

  -- Fills the parameter obj with all values from Self.
  procedure FillClone( Self : OPSMessage_Class; obj : OpsObject_Class_At ) is
  begin
    FillClone( OpsObject_Class(Self), obj );
    if obj.all in OPSMessage_Class'Class then
      OPSMessage_Class(obj.all).messageType := Self.messageType;
      OPSMessage_Class(obj.all).publisherPriority := Self.publisherPriority;
      OPSMessage_Class(obj.all).publicationID := Self.publicationID;
      Replace(OPSMessage_Class(obj.all).publisherName, Self.publisherName);
      Replace(OPSMessage_Class(obj.all).topicName, Self.topicName);
      Replace(OPSMessage_Class(obj.all).topLevelKey, Self.topLevelKey);
      Replace(OPSMessage_Class(obj.all).address, Self.address);
      if OPSMessage_Class(obj.all).data /= null then
        Free(OPSMessage_Class(obj.all).data);
      end if;
      if Self.data /= null then
        OPSMessage_Class(obj.all).data := Ops_Pa.OpsObject_Pa.OPSObject_Class_At(Clone(Self.data.all));
      end if;
    end if;
  end;

  procedure InitInstance( Self : in out OPSMessage_Class ) is
  begin
    InitInstance( OpsObject_Class(Self) );
    AppendType( OpsObject_Class(Self), "ops.protocol.OPSMessage" );
    Self.DataOwner := True;
  end;

  procedure Finalize( Self : in out OPSMessage_Class ) is
  begin
    -- Validation
    if Self.NrOfReservations /= 0 then
      StaticErrorService.
        Report(Error_Class_At(Create("OPSMessage", "Finalize", "Invalid reserve value @ delete. Mismatched Reserve/Unreserve calls!")));
    end if;

    if Self.publisherName /= null then
      Dispose(Self.publisherName);
    end if;
    if Self.topicName /= null then
      Dispose(Self.topicName);
    end if;
    if Self.topLevelKey /= null then
      Dispose(Self.topLevelKey);
    end if;
    if Self.address /= null then
      Dispose(Self.address);
    end if;
    if Self.DataOwner and Self.data /= null then
      Free(Self.data);
    end if;

    Finalize( OpsObject_Class(Self) );
  end;

end Ops_Pa.OpsObject_Pa.OPSMessage_Pa;

