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

with Ops_Pa.MemoryMap_Pa;
use Ops_Pa.MemoryMap_Pa;

package Ops_Pa.ByteBuffer_Pa is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type ByteBuffer_Class    is new Ops_Class with private;
  type ByteBuffer_Class_At is access all ByteBuffer_Class'Class;

  -- Constructors
  function Create( mMap : MemoryMap_Class_At ) return ByteBuffer_Class_At;

  -- Only valid for a ByteBuffer instance used to write data.
  -- Returns the the number of bytes containing valid data in the buffer so far.
  function GetSize( Self : ByteBuffer_Class ) return UInt32;

  -- Resets the whole buffer to creation state
  procedure Reset( Self : in out ByteBuffer_Class );

  -- Resets the internal offset pointer to 0 (zero)
  procedure ResetIndex( Self : in out ByteBuffer_Class );

  function GetNrOfSegments( Self : ByteBuffer_Class ) return UInt32;
  function GetSegmentSize( Self : ByteBuffer_Class; i : UInt32 ) return UInt32;
  function GetSegment( Self : ByteBuffer_Class; i : UInt32 ) return Byte_Arr_At;
  procedure Finish( Self : in out ByteBuffer_Class );

  procedure WriteChars( Self : in out ByteBuffer_Class; chars: Byte_Arr );
  procedure ReadChars( Self : in out ByteBuffer_Class; chars: out Byte_Arr );

  -- Writes the 4 bytes making up f to the buffer and increments index by 4.
  procedure WriteFloat( Self : in out ByteBuffer_Class; f : Float32 );
  -- Writes the 2 bytes making up i to the buffer and increments index by 2.
  procedure WriteShort( Self : in out ByteBuffer_Class; i : Int16 );
  --Writes the 4 bytes making up i to the buffer and increments index by 4.
  procedure WriteInt( Self : in out ByteBuffer_Class; i : Int32 );
  --Writes the 8 bytes making up l to the buffer and increments index by 8.
  procedure WriteLong( Self : in out ByteBuffer_Class; l : Int64 );
  --Writes the 8 bytes making up d to the buffer and increments index by 4.
  procedure WriteDouble( Self : in out ByteBuffer_Class; d : Float64 );
  --Writes c to the buffer and increments index by 1.
  procedure WriteChar( Self : in out ByteBuffer_Class; c : Byte );
  --Writes flag to the buffer and increments index by 1.
  procedure WriteBoolean( Self : in out ByteBuffer_Class; flag : Boolean );
  --Writes Length(s) followed by s to the buffer as a c-string (8-bit chars) and increments the buffer by Length(s) + 4.
  procedure WriteString( Self : in out ByteBuffer_Class; s : String_At );
  procedure WriteString( Self : in out ByteBuffer_Class; s : String );

  -- Reads 4 bytes from the buffer and returns them as a float. Index is increased by 4.
  procedure ReadFloat( Self : in out ByteBuffer_Class; Value : out Float32 );
  -- Reads 2 bytes from the buffer and returns them as an short. Index is increased by 2.
  procedure ReadShort( Self : in out ByteBuffer_Class; Value : out Int16 );
  -- Reads 4 bytes from the buffer and returns them as an int. Index is increased by 4.
  procedure ReadInt( Self : in out ByteBuffer_Class; Value : out Int32 );
  --Reads 8 bytes from the buffer and returns them as a long long (__int64). Index is increased by 8.
  procedure ReadLong( Self : in out ByteBuffer_Class; Value : out Int64 );
  -- Reads 8 bytes from the buffer and returns them as a double. Index is increased by 8.
  procedure ReadDouble( Self : in out ByteBuffer_Class; Value : out Float64 );
  -- Reads 1 byte from the buffer and returns it as a char. Index is increased by 1.
  procedure ReadChar( Self : in out ByteBuffer_Class; Value : out Byte );
  -- Reads 1 byte from the buffer and returns it as a boolean. Index is increased by 1.
  procedure ReadBoolean( Self : in out ByteBuffer_Class; Value : out Boolean );
  -- Reads an int (length) from the buffer followed by length number of chars returned as a string. Index is increased by length + 4.
  procedure ReadString( Self : in out ByteBuffer_Class; Value : out String_At );

  -- /////////////////////

  procedure WriteFloats( Self : in out ByteBuffer_Class; value : Float32_Arr );
  procedure WriteShorts( Self : in out ByteBuffer_Class; value : Int16_Arr );
  procedure WriteInts( Self : in out ByteBuffer_Class; value : Int32_Arr );
  procedure WriteLongs( Self : in out ByteBuffer_Class; value : Int64_Arr );
  procedure WriteDoubles( Self : in out ByteBuffer_Class; value : Float64_Arr );
  procedure WriteBytes( Self : in out ByteBuffer_Class; value : Byte_Arr );
  procedure WriteBooleans( Self : in out ByteBuffer_Class; value : Boolean_Arr );
  procedure WriteStrings( Self : in out ByteBuffer_Class; value : String_Arr );

  procedure ReadFloats( Self : in out ByteBuffer_Class; Value : out Float32_Arr_At );
  procedure ReadShorts( Self : in out ByteBuffer_Class; Value : out Int16_Arr_At );
  procedure ReadInts( Self : in out ByteBuffer_Class; Value : out Int32_Arr_At );
  procedure ReadLongs( Self : in out ByteBuffer_Class; Value : out Int64_Arr_At );
  procedure ReadDoubles( Self : in out ByteBuffer_Class; Value : out Float64_Arr_At );
  procedure ReadBytes( Self : in out ByteBuffer_Class; Value : out Byte_Arr_At );
  procedure ReadBooleans( Self : in out ByteBuffer_Class; Value : out Boolean_Arr_At );
  procedure ReadStrings( Self : in out ByteBuffer_Class; Value : out String_Arr_At );

  procedure ReadFloats( Self : in out ByteBuffer_Class; Value : out Float32_Arr );
  procedure ReadShorts( Self : in out ByteBuffer_Class; Value : out Int16_Arr );
  procedure ReadInts( Self : in out ByteBuffer_Class; Value : out Int32_Arr );
  procedure ReadLongs( Self : in out ByteBuffer_Class; Value : out Int64_Arr );
  procedure ReadDoubles( Self : in out ByteBuffer_Class; Value : out Float64_Arr );
  procedure ReadBytes( Self : in out ByteBuffer_Class; Value : out Byte_Arr );
  procedure ReadBooleans( Self : in out ByteBuffer_Class; Value : out Boolean_Arr );
  procedure ReadStrings( Self : in out ByteBuffer_Class; Value : out String_Arr );

  -- /////////////////////

  procedure WriteProtocol( Self : in out ByteBuffer_Class );
  procedure CheckProtocol( Self : in out ByteBuffer_Class; Result : out Boolean );

  procedure WriteNewSegment( Self : in out ByteBuffer_Class );

private
-- ==========================================================================
--
-- ==========================================================================
  type ByteBuffer_Class is new Ops_Class with
    record
      -- Buffer used to store data to be written or read.
      MemMap : MemoryMap_Class_At := null;

      -- index pointing out where in the buffer to do the next read or write.
      -- This variable is automatically incremented by the read and write  operations.
      Index : UInt32;

      TotalSize : UInt32;
      NextSegmentAt : UInt32;
      CurrentSegment : UInt32;
    end record;

  procedure ReadNewSegment( Self : in out ByteBuffer_Class );

  procedure InitInstance( Self : in out ByteBuffer_Class; mMap : MemoryMap_Class_At );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out ByteBuffer_Class );

end Ops_Pa.ByteBuffer_Pa;
