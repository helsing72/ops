--
-- Copyright (C) 2017-2020 Lennart Andersson.
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

with Ada.Containers.Indefinite_Ordered_Maps,
     Ops_Pa.Error_Pa;
use  Ops_Pa.Error_Pa;

package Ops_Pa.Transport_Pa.SendDataHandler_Pa.McUdp_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type McUdpSendDataHandler_Class    is new SendDataHandler_Class with private;
  type McUdpSendDataHandler_Class_At is access all McUdpSendDataHandler_Class'Class;

  function Create(localInterface : String; ttl : Integer; outSocketBufferSize : Int64; Reporter : ErrorService_Class_At) return McUdpSendDataHandler_Class_At;

  overriding function sendData( Self : in out McUdpSendDataHandler_Class; buf : Byte_Arr_At; bufSize : Integer; topic : Topic_Class_At) return Boolean;

  procedure addSink( Self : in out McUdpSendDataHandler_Class; topic : String; Ip : String; Port : Int32; staticRoute : Boolean);

private
-- ==========================================================================
--
-- ==========================================================================
  type IpPortPair_Class is new Ops_Class with
     record
       ip : String_At := null;
       port : Int32 := 0;
       alwaysAlive : Boolean := False;
       lastTimeAlive : TimeMs_T := 0;
     end record;
  type IpPortPair_Class_At is access all IpPortPair_Class'Class;

  function Create(ip : string; port : Int32; alwaysAlive : Boolean) return IpPortPair_Class_At;

  function isAlive( Self : IpPortPair_Class ) return Boolean;
  procedure feedWatchdog( Self : in out IpPortPair_Class; alwaysAlive : Boolean );
  function getKey( Self : IpPortPair_Class ) return String;

  function getKey( ip : String; Port : Int32 ) return String;

  procedure InitInstance( Self : in out IpPortPair_Class;
                          ip : String;
                          port : Int32;
                          alwaysAlive : Boolean );

  overriding procedure Finalize( Self : in out IpPortPair_Class );

  -- ==============================

  -- Define a map that can hold IpPortPairs, key is made up of ip and port using method getKey above
  function Less (Left, Right : String) return Boolean;
  function Equal (Left, Right : IpPortPair_Class_At) return Boolean;

  package IpPortPairMap is new Ada.Containers.Indefinite_Ordered_Maps(String, IpPortPair_Class_At, Less, Equal);

  type TopicMap is
     record
       staticRoute : Boolean := False;
       Map : IpPortPairMap.Map;
     end record;

  type TopicMap_At is access all TopicMap;

  -- ==============================

  -- Define a map that can hold IpPortPairMaps, key is a topic name
  function Equal (Left, Right : TopicMap_At) return Boolean;

  package IpPortPairDict is new Ada.Containers.Indefinite_Ordered_Maps(String, TopicMap_At, Less, Equal);


-- ==========================================================================
--
-- ==========================================================================
  type McUdpSendDataHandler_Class    is new SendDataHandler_Class with
     record
       TopSinkMap : IpPortPairDict.Map;
     end record;

  procedure InitInstance( Self : in out McUdpSendDataHandler_Class;
                          SelfAt : McUdpSendDataHandler_Class_At;
                          localInterface : String;
                          ttl : Integer;
                          outSocketBufferSize : Int64;
                          Reporter : ErrorService_Class_At );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out McUdpSendDataHandler_Class );

end Ops_Pa.Transport_Pa.SendDataHandler_Pa.McUdp_Pa;

