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

with Ops_Pa.OpsObject_Pa.OPSMessage_Pa;
with Ops_Pa.OpsObject_Pa.Transport_Pa;
with Ops_Pa.OpsObject_Pa.Channel_Pa;
with Ops_Pa.OpsObject_Pa.Topic_Pa;
with Ops_Pa.OpsObject_Pa.Domain_Pa;
with Ops_Pa.OpsObject_Pa.OPSConfig_Pa;
with Ops_Pa.OpsObject_Pa.TopicInfoData_Pa;
with Ops_Pa.OpsObject_Pa.ParticipantInfoData_Pa;

package body Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.OpsObjectFactory_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type BuiltInFactory_Class    is new SerializableFactory_Class with null record;
  type BuiltInFactory_Class_At is access all BuiltInFactory_Class'Class;

  -- Constructors
  function Create return BuiltInFactory_Class_At;

  -- Create a serializable class instance from given type
  function Make( Self : BuiltInFactory_Class; types : string) return Serializable_Class_At;

  procedure Finalize( Self : in out BuiltInFactory_Class );

  -- Constructors
  function Create return BuiltInFactory_Class_At is
    Self : BuiltInFactory_Class_At := null;
  begin
    Self := new BuiltInFactory_Class;
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  function Make( Self : BuiltInFactory_Class; types : string) return Serializable_Class_At is
  begin
    if types = "ops.protocol.OPSMessage" then
      return Serializable_Class_At(Ops_Pa.OpsObject_Pa.OPSMessage_Pa.Create);
    elsif types = "Topic" then
      return Serializable_Class_At(Ops_Pa.OpsObject_Pa.Topic_Pa.Create);
    elsif types = "Channel" then
      return Serializable_Class_At(Ops_Pa.OpsObject_Pa.Channel_Pa.Create);
    elsif types = "Transport" then
      return Serializable_Class_At(Ops_Pa.OpsObject_Pa.Transport_Pa.Create);
    elsif types = "DefaultOPSConfigImpl" then
      declare
        res : Ops_Pa.OpsObject_Pa.OPSConfig_Pa.DefaultOPSConfigImpl_Class_At := Ops_Pa.OpsObject_Pa.OPSConfig_Pa.Create;
      begin
        return Serializable_Class_At(res);
      end;
    elsif types = "MulticastDomain" then
      return Serializable_Class_At(Ops_Pa.OpsObject_Pa.Domain_Pa.Create);
    elsif types = "Domain" then
      return Serializable_Class_At(Ops_Pa.OpsObject_Pa.Domain_Pa.Create);
    elsif types = "ops.ParticipantInfoData" then
      return Serializable_Class_At(Ops_Pa.OpsObject_Pa.ParticipantInfoData_Pa.Create);
    elsif types = "TopicInfoData" then
      return Serializable_Class_At(Ops_Pa.OpsObject_Pa.TopicInfoData_Pa.Create);
    end if;
    return null;
  end;

  procedure Finalize( Self : in out BuiltInFactory_Class ) is
  begin
    null;
  end;

  -- -----------------------------------------------------------------------
  --
  -- -----------------------------------------------------------------------

  -- Constructors
  function Create return OPSObjectFactoryImpl_Class_At is
    Self : OPSObjectFactoryImpl_Class_At := null;
  begin
    Self := new OPSObjectFactoryImpl_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  procedure InitInstance( Self : in out OPSObjectFactoryImpl_Class ) is
    fact : BuiltInFactory_Class_At := Create;
  begin
    InitInstance( OPSObjectFactory_Class(Self) );
    Self.Add(SerializableFactory_Class_At(fact));
  end;

  procedure Finalize( Self : in out OPSObjectFactoryImpl_Class ) is
  begin
    Finalize( OPSObjectFactory_Class(Self) );
  end;

  -- -----------------------------------------------------------------------
  --
  -- -----------------------------------------------------------------------

  -- Create the singleton instance of our factory
  gInstance : OPSObjectFactoryImpl_Class_At := Create;

  -- Return singelton instance of OPSObjectFactory.
  function getInstance return OPSObjectFactory_Class_At is
  begin
    return OPSObjectFactory_Class_At(gInstance);
  end;

  procedure Finalize( Self : in out OPSObjectFactory_Class ) is
  begin
    Finalize( SerializableInheritingTypeFactory_Class(Self) );
  end;

end Ops_Pa.SerializableFactory_Pa.CompFactory_Pa.OpsObjectFactory_Pa;

