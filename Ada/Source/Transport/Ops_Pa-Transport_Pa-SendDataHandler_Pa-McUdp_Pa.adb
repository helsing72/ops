--
-- Copyright (C) 2017 Lennart Andersson.
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

with Ada.Text_IO; use Ada.Text_IO;
with Ada.Containers.Indefinite_Vectors;

package body Ops_Pa.Transport_Pa.SendDataHandler_Pa.McUdp_Pa is

  use type IpPortPairMap.cursor;
  use type IpPortPairDict.cursor;

  ALIVE_TIMEOUT : constant TimeMs_T := 3000;

  function Less (Left, Right : String) return Boolean is
  begin
    return Left < Right;
  end;

  function Equal (Left, Right : IpPortPair_Class_At) return Boolean is
  begin
    return Left = Right;
  end;

  function Equal (Left, Right : TopicMap_At) return Boolean is
  begin
    return Left = Right;
  end;

  function Create(ip : string; port : Int32) return IpPortPair_Class_At is
    Self : IpPortPair_Class_At := null;
  begin
    Self := new IpPortPair_Class;
    InitInstance( Self.all, ip, port );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  function isAlive( Self : IpPortPair_Class ) return Boolean is
  begin
    return (GetTimeInMs - Self.lastTimeAlive) < ALIVE_TIMEOUT;
  end;

  procedure feedWatchdog( Self : in out IpPortPair_Class ) is
  begin
    Self.lastTimeAlive := GetTimeInMs;
  end;

  function getKey( Self : IpPortPair_Class ) return String is
  begin
    return getKey(Self.ip.all, Self.port);
  end;

  function getKey(ip : String; Port : Int32) return String is
  begin
    return ip & ':' & Int32'Image(port);
  end;

  procedure InitInstance( Self : in out IpPortPair_Class;
                          ip : String;
                          port : Int32 ) is
  begin
    Self.ip := Copy(ip);
    Self.port := port;
    Self.lastTimeAlive := GetTimeInMs;
  end;

  overriding procedure Finalize( Self : in out IpPortPair_Class ) is
  begin
    Dispose(Self.ip);
  end;

  function Create(localInterface : String; ttl : Integer; outSocketBufferSize : Int64; Reporter : ErrorService_Class_At) return McUdpSendDataHandler_Class_At is
    Self : McUdpSendDataHandler_Class_At := null;
  begin
    Self := new McUdpSendDataHandler_Class;
    InitInstance( Self.all, localInterface, ttl, outSocketBufferSize, Reporter );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end;

  function Equal(Left, Right : String) return Boolean;

  package Vect is new Ada.Containers.Indefinite_Vectors(Positive, String, Equal);

  function Equal(Left, Right : String) return Boolean is
  begin
    return Left = Right;
  end;

  overriding function sendData( Self : in out McUdpSendDataHandler_Class; buf : Byte_Arr_At; bufSize : Integer; topic : Topic_Class_At) return Boolean is
    Result : Boolean := True;
    ipPortMap : TopicMap_At := null;
    sinksToDelete : Vect.Vector;

    procedure SendData(Position : in IpPortPairMap.Cursor) is
      ipPort : IpPortPair_Class_At := null;
    begin
      ipPort := IpPortPairMap.Element( Position );
      -- Check if this sink is alive
      if ipPort.isAlive then
        Result := Result and Self.Sender.sendTo(buf, bufSize, ipPort.ip.all, Integer(ipPort.port));
      else
        -- Mark it for deletion
        --Put_Line( topic.Name & " removing " & ipPort.getKey);
        sinksToDelete.Append(ipPort.getKey);
      end if;
    end;

    procedure DeleteMarked(Position : Vect.Cursor ) is
      Key : String := Vect.Element( Position );
      posMap : IpPortPairMap.Cursor;
      ipPort : IpPortPair_Class_At := null;
    begin
      posMap := ipPortMap.Map.Find( key );
      if posMap /= IpPortPairMap.No_Element then
        ipPort := IpPortPairMap.Element( posMap );
        Free( ipPort );
        ipPortMap.Map.Delete( Key );
      end if;
    end;

    posDict : IpPortPairDict.Cursor;
    S : Com_Mutex_Pa.Scope_Lock(Self.Mutex'Access);

  begin
    posDict := Self.TopSinkMap.Find( topic.Name );
    if posDict /= IpPortPairDict.No_Element then
      ipPortMap := IpPortPairDict.Element( posDict );

      -- Loop over all sinks and send data, mark items that isn't "alive".
      ipPortMap.Map.Iterate( SendData'Access );

      -- Delete all IpPortPair's that is marked for delete
      sinksToDelete.Iterate( DeleteMarked'Access );
    end if;
    return Result;
  end;

  procedure addSink( Self : in out McUdpSendDataHandler_Class; topic : String; Ip : String; Port : Int32) is
    ipPort : IpPortPair_Class_At := null;
    ipPortMap : TopicMap_At := null;
    posMap : IpPortPairMap.Cursor;
    posDict : IpPortPairDict.Cursor;
    S : Com_Mutex_Pa.Scope_Lock(Self.Mutex'Access);
  begin
    -- check if we already have a sink map for this topic
    posDict := Self.TopSinkMap.Find( topic );
    if posDict = IpPortPairDict.No_Element then
      -- We have no sink map for this topic, so add it
      ipPortMap := new TopicMap;
      Self.TopSinkMap.Insert( topic, ipPortMap );
    else
      ipPortMap := IpPortPairDict.Element( posDict );
    end if;

    posMap := ipPortMap.Map.Find( getKey( Ip, Port ) );
    if posMap = IpPortPairMap.No_Element then
      -- We have no matching sink, add it
      ipPort := Create(Ip, Port);
      ipPortMap.Map.Insert( ipPort.getKey, ipPort );
      --Put_Line( topic & " added as new sink " & ipPort.getKey);
    else
      ipPort := IpPortPairMap.Element( posMap );
    end if;

    ipPort.feedWatchdog;
  end;

  procedure InitInstance( Self : in out McUdpSendDataHandler_Class;
                          localInterface : String;
                          ttl : Integer;
                          outSocketBufferSize : Int64;
                          Reporter : ErrorService_Class_At ) is
  begin
    InitInstance( SendDataHandler_Class(Self) );
    Self.Sender := createUDPSender(localInterface, ttl, OutSocketBufferSize);
    Self.Sender.SetErrorService( Reporter );
  end;

  overriding procedure Finalize( Self : in out McUdpSendDataHandler_Class ) is

    procedure ProcessIpPortPair(Position : IpPortPairMap.Cursor ) is
    begin
      Free( IpPortPairMap.Element( Position ) );
    end;

    procedure Dispose is new Ada.Unchecked_Deallocation( TopicMap, TopicMap_At );

    procedure Process( Position : IpPortPairDict.Cursor ) is
      ipPortMap : TopicMap_At := IpPortPairDict.Element( Position );
    begin
      ipPortMap.Map.Iterate( ProcessIpPortPair'Access );
      Dispose(ipPortMap);
    end;

  begin
    declare
      S : Com_Mutex_Pa.Scope_Lock(Self.Mutex'Access);
    begin
      Self.TopSinkMap.Iterate( Process'Access );
      Free(Self.Sender);
    end;
    Finalize( SendDataHandler_Class(Self) );
  end;

end Ops_Pa.Transport_Pa.SendDataHandler_Pa.McUdp_Pa;

