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

package body Ops_Pa.MemoryMap_Pa is

  function Create( Width : UInt32; Height : UInt32 ) return MemoryMap_Class_At is
    Self : MemoryMap_Class_At := null;
  begin
    Self := new MemoryMap_Class;
    InitInstance( Self.all, Width, Height );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  function Create( Segment : Byte_Arr_At; Size : UInt32 ) return MemoryMap_Class_At is
    Self : MemoryMap_Class_At := null;
  begin
    Self := new MemoryMap_Class;
    InitInstance( Self.all, Segment, Size );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  procedure ChangeBuffer( Self : in out MemoryMap_Class; Segment : Byte_Arr_At; Size : UInt32 ) is
  begin
    if not Self.DataCreator then
      Self.Height := Size;
      Self.Segments(0) := Segment;
    end if;
  end ChangeBuffer;

  function GetSegment( Self : MemoryMap_Class; i : UInt32 ) return Byte_Arr_At is
  begin
    if i < Self.Width then
      return Self.Segments(i);
    end if;
    return null;
  end GetSegment;

  procedure CopyToBytes( Self : MemoryMap_Class; Dest : Byte_Arr_At; StartIndex : UInt32; EndIndex : UInt32 ) is
    bytesToCopy : UInt32 := EndIndex - StartIndex + 1;
    currentSegment : UInt32 := StartIndex / SegmentSize( Self );
    indexInSegment : UInt32 := StartIndex - (currentSegment +  1) * SegmentSize( Self );
    bytesLeftInSegment : UInt32 := SegmentSize( Self ) - indexInSegment;
  begin
    null;
    if bytesLeftInSegment >= bytesToCopy then
      raise Not_Yet_Implemented;
--  			memcpy(dest, getSegment(currentSegment) + indexInSegment, bytesToCopy);
    else
      -- Recursively call this method again until we copied all bytes.
      raise Not_Yet_Implemented;
--  			memcpy(dest, getSegment(currentSegment) + indexInSegment, bytesLeftInSegment);
--  			copyToBytes(dest + bytesLeftInSegment, startIndex + bytesLeftInSegment, endIndex);
    end if;
  end CopyToBytes;

  function SegmentSize( Self : MemoryMap_Class ) return UInt32 is
  begin
    return Self.Height;
  end;

  function NrOfSegments( Self : MemoryMap_Class ) return UInt32 is
  begin
    return Self.Width;
  end;

  function TotalSize( Self : MemoryMap_Class ) return UInt32 is
  begin
    return Self.Height * Self.Width;
  end;

  procedure InitInstance( Self : in out MemoryMap_Class; Width : UInt32; Height : UInt32 ) is
  begin
    Self.Width := Width;
    Self.Height := Height;
    Self.DataCreator := True;

    -- Allocate segment pointer array
    Self.Segments := new Segment_Arr'(0..Width-1 => null);

    -- Allocate all segments
    for i in Self.Segments'Range loop
      Self.Segments(i) := new Byte_Arr'(0..Byte_Arr_Index_T(Height)-1 => 0);
    end loop;
  end InitInstance;

  procedure InitInstance( Self : in out MemoryMap_Class; Segment : Byte_Arr_At; Size : UInt32 ) is
  begin
    Self.Width := 1;
    Self.Height := Size;
    Self.DataCreator := False;

    -- Allocate segment pointer array
    Self.Segments := new Segment_Arr'(0..Self.Width-1 => Segment);
  end InitInstance;

  overriding procedure Finalize( Self : in out MemoryMap_Class ) is
  begin
    -- Delete all data.
    if Self.DataCreator then
      for i in Self.Segments'Range loop
        Dispose(Self.Segments(i));
      end loop;
    end if;
    Dispose(Self.Segments);
  end;

end Ops_Pa.MemoryMap_Pa;
