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

package Ops_Pa.MemoryMap_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type MemoryMap_Class    is new Ops_Class with private;
  type MemoryMap_Class_At is access all MemoryMap_Class'Class;

  -- Constructors
  function Create( Width : UInt32; Height : UInt32 ) return MemoryMap_Class_At;
  function Create( Segment : Byte_Arr_At; Size : UInt32 ) return MemoryMap_Class_At;

  -- Only valid to call for a memorymap created with a buffer and size
  procedure ChangeBuffer( Self : in out MemoryMap_Class; Segment : Byte_Arr_At; Size : UInt32 );

  function GetSegment( Self : MemoryMap_Class; i : UInt32 ) return Byte_Arr_At;

  procedure CopyToBytes( Self : MemoryMap_Class; Dest : Byte_Arr_At; StartIndex : UInt32; EndIndex : UInt32 );

  function SegmentSize( Self : MemoryMap_Class ) return UInt32;
  function NrOfSegments( Self : MemoryMap_Class ) return UInt32;
  function TotalSize( Self : MemoryMap_Class ) return UInt32;

private
-- ==========================================================================
--
-- ==========================================================================

  type Segment_Arr is array(UInt32 range <>) of Byte_Arr_At;
  type Segment_Arr_At is access all Segment_Arr;

  procedure Dispose is new Ada.Unchecked_Deallocation( Segment_Arr, Segment_Arr_At );

  type MemoryMap_Class is new Ops_Class with
    record
      Width : UInt32 := 0;
      Height : UInt32 := 0;
      DataCreator : Boolean := False;
      Segments : Segment_Arr_At := null;
    end record;

  procedure InitInstance( Self : in out MemoryMap_Class; Width : UInt32; Height : UInt32 );
  procedure InitInstance( Self : in out MemoryMap_Class; Segment : Byte_Arr_At; Size : UInt32 );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out MemoryMap_Class );

end Ops_Pa.MemoryMap_Pa;

