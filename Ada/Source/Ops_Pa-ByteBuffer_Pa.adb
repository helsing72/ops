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

with Ada.Unchecked_Conversion;

package body Ops_Pa.ByteBuffer_Pa is

  subtype ByteArr2_T is Byte_Arr(0..1);
  subtype ByteArr4_T is Byte_Arr(0..3);
  subtype ByteArr8_T is Byte_Arr(0..7);

  function ToByte is new Ada.Unchecked_Conversion(Character, Byte);

  function ToByteArr is new Ada.Unchecked_Conversion(Int16, ByteArr2_T);
  function ToByteArr is new Ada.Unchecked_Conversion(Int32, ByteArr4_T);
  function ToByteArr is new Ada.Unchecked_Conversion(Int64, ByteArr8_T);
  function ToByteArr is new Ada.Unchecked_Conversion(Float32, ByteArr4_T);
  function ToByteArr is new Ada.Unchecked_Conversion(Float64, ByteArr8_T);

  function FromByte is new Ada.Unchecked_Conversion(Byte, Character);

  function FromByteArr is new Ada.Unchecked_Conversion(ByteArr2_T, Int16);
  function FromByteArr is new Ada.Unchecked_Conversion(ByteArr4_T, Int32);
  function FromByteArr is new Ada.Unchecked_Conversion(ByteArr8_T, Int64);
  function FromByteArr is new Ada.Unchecked_Conversion(ByteArr4_T, Float32);
  function FromByteArr is new Ada.Unchecked_Conversion(ByteArr8_T, Float64);

  -- Constructors
  function Create( mMap : MemoryMap_Class_At ) return ByteBuffer_Class_At is
    Self : ByteBuffer_Class_At := null;
  begin
    Self := new ByteBuffer_Class;
    InitInstance( Self.all, mMap );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  -- Only valid for a ByteBuffer instance used to write data.
  -- Returns the the number of bytes containing valid data in the buffer so far.
  function GetSize( Self : ByteBuffer_Class ) return UInt32 is
  begin
    return Self.TotalSize;
  end;

  -- Resets the whole buffer to creation state
  procedure Reset( Self : in out ByteBuffer_Class ) is
  begin
    Self.Index  := 0;
    Self.TotalSize := 0;
    Self.CurrentSegment := 0;
    Self.NextSegmentAt := Self.MemMap.SegmentSize;
  end;

  -- Resets the internal offset pointer to 0 (zero)
  procedure ResetIndex( Self : in out ByteBuffer_Class ) is
  begin
    Self.Index := 0;
  end;

  function GetNrOfSegments( Self : ByteBuffer_Class ) return UInt32 is
  begin
    return Self.CurrentSegment + 1;
  end;

  function GetSegmentSize( Self : ByteBuffer_Class; i : UInt32 ) return UInt32 is
  begin
    if i < Self.CurrentSegment then
      return Self.MemMap.SegmentSize;
    else
      return Self.Index;
    end if;
  end;

  function GetSegment( Self : ByteBuffer_Class; i : UInt32 ) return Byte_Arr_At is
  begin
    return Self.MemMap.GetSegment(i);
  end;

  procedure Finish( Self : in out ByteBuffer_Class ) is
    oldIndex, oldNextSegmentAt, nrSeg : UInt32;
  begin
    oldIndex := Self.Index;
    oldNextSegmentAt := Self.NextSegmentAt;
    Self.NextSegmentAt := 0;
    nrSeg := Self.GetNrOfSegments;
    for i in 0 .. nrSeg - 1 loop
      Self.Index := 6;
      Self.CurrentSegment := i;
      Self.NextSegmentAt := Self.NextSegmentAt + Self.MemMap.SegmentSize;
      Self.WriteInt(Int32(nrSeg));
    end loop;
    Self.Index := oldIndex;
    Self.NextSegmentAt := oldNextSegmentAt;
  end;

  procedure WriteChars( Self : in out ByteBuffer_Class; chars: Byte_Arr ) is
    bytesLeftInSegment : Uint32;
    length : UInt32;
  begin
    bytesLeftInSegment := Self.MemMap.SegmentSize - Self.Index;
    length := chars'Length;
    if bytesLeftInSegment >= length then
      Self.MemMap.GetSegment(Self.CurrentSegment).all(Integer(Self.Index)..Integer(Self.Index+length-1)) := chars;
      Self.Index := Self.Index + length;
      Self.TotalSize := Self.TotalSize + length;
    else
      Self.MemMap.GetSegment(Self.CurrentSegment).all(Integer(Self.Index)..Integer(Self.Index+bytesLeftInSegment-1)) :=
        chars(chars'First..chars'First+Integer(bytesLeftInSegment-1));
      Self.Index := Self.Index + bytesLeftInSegment;
      Self.TotalSize := Self.TotalSize + bytesLeftInSegment;
      Self.NextSegmentAt := Self.NextSegmentAt + Self.MemMap.SegmentSize;
      Self.CurrentSegment := Self.CurrentSegment + 1;
      Self.WriteNewSegment;
      Self.WriteChars( chars(chars'First+Integer(bytesLeftInSegment)..chars'Last) );
    end if;
  end WriteChars;

  procedure ReadChars( Self : in out ByteBuffer_Class; chars: out Byte_Arr ) is
    bytesLeftInSegment : UInt32;
    length : UInt32;
  begin
    bytesLeftInSegment := Self.MemMap.SegmentSize - Self.Index;
    length := chars'Length;
    if bytesLeftInSegment >= length then
      chars := Self.MemMap.GetSegment(Self.CurrentSegment).all(Integer(Self.Index)..Integer(Self.Index+length-1));
      Self.Index := Self.Index + length;
      Self.TotalSize := Self.TotalSize + length;
    else
      chars(chars'First..chars'First+Integer(bytesLeftInSegment-1)) :=
        Self.MemMap.GetSegment(Self.CurrentSegment).all(Integer(Self.Index)..Integer(Self.Index+bytesLeftInSegment-1));
      Self.Index := Self.Index + bytesLeftInSegment;
      Self.TotalSize := Self.TotalSize + bytesLeftInSegment;
      Self.CurrentSegment := Self.CurrentSegment + 1;
      Self.ReadNewSegment;
      Self.ReadChars( chars(chars'First+Integer(bytesLeftInSegment)..chars'Last) );
    end if;
  end;

  procedure WriteNewSegment( Self : in out ByteBuffer_Class ) is
    tInt : Int32;
  begin
    Self.Index := 0;
    Self.WriteProtocol;
    tInt := 0;
    Self.WriteInt( tInt );
    Self.WriteInt( Int32(Self.CurrentSegment) );
  end;

  procedure ReadNewSegment( Self : in out ByteBuffer_Class ) is
    Flag : Boolean;
    Dummy : Int32;
  begin
    Self.Index := 0;
    Self.NextSegmentAt := Self.NextSegmentAt + Self.MemMap.SegmentSize;
    Self.CheckProtocol( Flag );
    Self.ReadInt( Dummy );
    Self.ReadInt( Dummy );
  end;

  -- Writes the 4 bytes making up f to the buffer and increments index by 4.
  procedure WriteFloat( Self : in out ByteBuffer_Class; f : Float32 ) is
  begin
    Self.WriteChars( ToByteArr(f) );
  end;

  -- Reads 4 bytes from the buffer and returns them as a float. Index is increased by 4.
  procedure ReadFloat( Self : in out ByteBuffer_Class; Value : out Float32 ) is
    arr : ByteArr4_T;
  begin
    Self.ReadChars( arr );
    Value := FromByteArr( arr );
 end;

  -- Writes the 2 bytes making up i to the buffer and increments index by 2.
  procedure WriteShort( Self : in out ByteBuffer_Class; i : Int16 ) is
  begin
    Self.WriteChars( ToByteArr(i) );
  end;

  -- Reads 2 bytes from the buffer and returns them as an short. Index is increased by 2.
  procedure ReadShort( Self : in out ByteBuffer_Class; Value : out Int16 ) is
    arr : ByteArr2_T;
  begin
    Self.ReadChars( arr );
    Value := FromByteArr( arr );
  end;

  --Writes the 4 bytes making up i to the buffer and increments index by 4.
  procedure WriteInt( Self : in out ByteBuffer_Class; i : Int32 ) is
  begin
    Self.WriteChars( ToByteArr(i) );
  end;

  -- Reads 4 bytes from the buffer and returns them as an int. Index is increased by 4.
  procedure ReadInt( Self : in out ByteBuffer_Class; Value : out Int32 ) is
    arr : ByteArr4_T;
  begin
    Self.ReadChars( arr );
    Value := FromByteArr( arr );
  end;

  --Writes the 8 bytes making up l to the buffer and increments index by 8.
  procedure WriteLong( Self : in out ByteBuffer_Class; l : Int64 ) is
  begin
    Self.WriteChars( ToByteArr(l) );
  end;

  --Reads 8 bytes from the buffer and returns them as a long long (__int64). Index is increased by 8.
  procedure ReadLong( Self : in out ByteBuffer_Class; Value : out Int64 ) is
    arr : ByteArr8_T;
  begin
    Self.ReadChars( arr );
    Value := FromByteArr( arr );
  end;

  --Writes the 8 bytes making up d to the buffer and increments index by 4.
  procedure WriteDouble( Self : in out ByteBuffer_Class; d : Float64 ) is
  begin
    Self.WriteChars( ToByteArr(d) );
  end;

  -- Reads 8 bytes from the buffer and returns them as a double. Index is increased by 8.
  procedure ReadDouble( Self : in out ByteBuffer_Class; Value : out Float64 ) is
    arr : ByteArr8_T;
  begin
    Self.ReadChars( arr );
    Value := FromByteArr( arr );
  end;

  --Writes c to the buffer and increments index by 1.
  procedure WriteChar( Self : in out ByteBuffer_Class; c : Byte ) is
    arr : Byte_Arr(1..1) := (1 => c);
  begin
    Self.WriteChars( arr );
  end;

  -- Reads 1 byte from the buffer and returns it as a char. Index is increased by 1.
  procedure ReadChar( Self : in out ByteBuffer_Class; Value : out Byte ) is
    arr : Byte_Arr(1..1);
  begin
    Self.ReadChars( arr );
    Value := arr(1);
  end;

  --Writes flag to the buffer and increments index by 1.
  procedure WriteBoolean( Self : in out ByteBuffer_Class; flag : Boolean ) is
    Bool_C : array(Boolean) of Byte := (0, 1);
  begin
    Self.WriteChar( Bool_C(flag) );
  end;

  -- Reads 1 byte from the buffer and returns it as a boolean. Index is increased by 1.
  procedure ReadBoolean( Self : in out ByteBuffer_Class; Value : out Boolean ) is
    b : Byte;
  begin
    Self.ReadChar( b );
    Value := b /= 0;
  end;

  --Writes Length(s) followed by s to the buffer as a c-string (8-bit chars) and increments the buffer by Length(s) + 4.
  procedure WriteString( Self : in out ByteBuffer_Class; s : String_At ) is
  begin
    if s = null then
      Self.WriteInt( 0 );
    else
      declare
        arr : Byte_Arr(1..s.all'Length);
      begin
        Self.WriteInt( s.all'Length );
        for I in s.all'range loop
          arr(I) := ToByte( s.all(I) );
        end loop;
        Self.WriteChars( arr );
      end;
    end if;
  end;

  -- Reads an int (length) from the buffer followed by length number of chars returned as a string. Index is increased by length + 4.
  procedure ReadString( Self : in out ByteBuffer_Class; Value : out String_At ) is
    Len : Int32;
  begin
    Self.ReadInt( Len );
    if Len <= 0 then
      Value := null;
    else
      declare
        arr : Byte_Arr(1..Integer(Len));
      begin
        Self.ReadChars( arr );
        Value := new String(arr'Range);
        for I in Value'Range loop
          Value(I) := FromByte( arr(I) );
        end loop;
      end;
    end if;
  end;

  procedure WriteFloats( Self : in out ByteBuffer_Class; value : Float32_Arr ) is
  begin
    Self.WriteInt( Int32(Value'Length) );
    if Value'Length > 0 then
      for I in Value'Range loop
        Self.WriteFloat( Value(I) );
      end loop;
    end if;
  end;

  procedure ReadFloats( Self : in out ByteBuffer_Class; Value : out Float32_Arr_At ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size > 0 then
      Value := new Float32_Arr(0..Integer(Size-1));
      for I in Value.all'Range loop
        Self.ReadFloat( Value(I) );
      end loop;
    else
      Value := null;
    end if;
  end;

  procedure ReadFloats( Self : in out ByteBuffer_Class; Value : out Float32_Arr ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size /= Value'Length then
      raise Illegal_Array_Length;
    end if;
    for I in Value'Range loop
      Self.ReadFloat( Value(I) );
    end loop;
  end;

  procedure WriteShorts( Self : in out ByteBuffer_Class; value : Int16_Arr ) is
  begin
    Self.WriteInt( Int32(Value'Length) );
    if Value'Length > 0 then
      for I in Value'Range loop
        Self.WriteShort( Value(I) );
      end loop;
    end if;
  end;

  procedure ReadShorts( Self : in out ByteBuffer_Class; Value : out Int16_Arr_At ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size > 0 then
      Value := new Int16_Arr(0..Integer(Size-1));
      for I in Value.all'Range loop
        Self.ReadShort( Value(I) );
      end loop;
    else
      Value := null;
    end if;
  end;

  procedure ReadShorts( Self : in out ByteBuffer_Class; Value : out Int16_Arr ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size /= Value'Length then
      raise Illegal_Array_Length;
    end if;
    for I in Value'Range loop
      Self.ReadShort( Value(I) );
    end loop;
  end;

  procedure WriteInts( Self : in out ByteBuffer_Class; value : Int32_Arr ) is
  begin
    Self.WriteInt( Int32(Value'Length) );
    if Value'Length > 0 then
      for I in Value'Range loop
        Self.WriteInt( Value(I) );
      end loop;
    end if;
  end;

  procedure ReadInts( Self : in out ByteBuffer_Class; Value : out Int32_Arr_At ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size > 0 then
      Value := new Int32_Arr(0..Integer(Size-1));
      for I in Value.all'Range loop
        Self.ReadInt( Value(I) );
      end loop;
    else
      Value := null;
    end if;
  end;

  procedure ReadInts( Self : in out ByteBuffer_Class; Value : out Int32_Arr ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size /= Value'Length then
      raise Illegal_Array_Length;
    end if;
    for I in Value'Range loop
      Self.ReadInt( Value(I) );
    end loop;
  end;

  procedure WriteLongs( Self : in out ByteBuffer_Class; value : Int64_Arr ) is
  begin
    Self.WriteInt( Int32(Value'Length) );
    if Value'Length > 0 then
      for I in Value'Range loop
        Self.WriteLong( Value(I) );
      end loop;
    end if;
  end;

  procedure ReadLongs( Self : in out ByteBuffer_Class; Value : out Int64_Arr_At ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size > 0 then
      Value := new Int64_Arr(0..Integer(Size-1));
      for I in Value.all'Range loop
        Self.ReadLong( Value(I) );
      end loop;
    else
      Value := null;
    end if;
  end;

  procedure ReadLongs( Self : in out ByteBuffer_Class; Value : out Int64_Arr ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size /= Value'Length then
      raise Illegal_Array_Length;
    end if;
    for I in Value'Range loop
      Self.ReadLong( Value(I) );
    end loop;
  end;

  procedure WriteDoubles( Self : in out ByteBuffer_Class; value : Float64_Arr ) is
  begin
    Self.WriteInt( Int32(Value'Length) );
    if Value'Length > 0 then
      for I in Value'Range loop
        Self.WriteDouble( Value(I) );
      end loop;
    end if;
  end;

  procedure ReadDoubles( Self : in out ByteBuffer_Class; Value : out Float64_Arr_At ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size > 0 then
      Value := new Float64_Arr(0..Integer(Size-1));
      for I in Value.all'Range loop
        Self.ReadDouble( Value(I) );
      end loop;
    else
      Value := null;
    end if;
  end;

  procedure ReadDoubles( Self : in out ByteBuffer_Class; Value : out Float64_Arr ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size /= Value'Length then
      raise Illegal_Array_Length;
    end if;
    for I in Value'Range loop
      Self.ReadDouble( Value(I) );
    end loop;
  end;

  procedure WriteBytes( Self : in out ByteBuffer_Class; value : Byte_Arr ) is
  begin
    Self.WriteInt( Int32(Value'Length) );
    if Value'Length > 0 then
      Self.WriteChars( Value );
    end if;
  end;

  procedure ReadBytes( Self : in out ByteBuffer_Class; Value : out Byte_Arr_At ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size > 0 then
      Value := new Byte_Arr(0..Integer(Size-1));
      Self.ReadChars( Value.all );
    else
      Value := null;
    end if;
  end;

  procedure ReadBytes( Self : in out ByteBuffer_Class; Value : out Byte_Arr ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size /= Value'Length then
      raise Illegal_Array_Length;
    end if;
    Self.ReadChars( Value );
  end;

  procedure WriteBooleans( Self : in out ByteBuffer_Class; value : Boolean_Arr ) is
  begin
    Self.WriteInt( Int32(Value'Length) );
    if Value'Length > 0 then
      for I in Value'Range loop
        Self.WriteBoolean( Value(I) );
      end loop;
    end if;
  end;

  procedure ReadBooleans( Self : in out ByteBuffer_Class; Value : out Boolean_Arr_At ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size > 0 then
      Value := new Boolean_Arr(0..Integer(Size-1));
      for I in Value.all'Range loop
        Self.ReadBoolean( Value(I) );
      end loop;
    else
      Value := null;
    end if;
  end;

  procedure ReadBooleans( Self : in out ByteBuffer_Class; Value : out Boolean_Arr ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size /= Value'Length then
      raise Illegal_Array_Length;
    end if;
    for I in Value'Range loop
      Self.ReadBoolean( Value(I) );
    end loop;
  end;

  procedure WriteStrings( Self : in out ByteBuffer_Class; value : String_Arr ) is
  begin
    Self.WriteInt( Int32(Value'Length) );
    if Value'Length > 0 then
      for I in Value'Range loop
        Self.WriteString( Value(I) );
      end loop;
    end if;
  end;

  procedure ReadStrings( Self : in out ByteBuffer_Class; Value : out String_Arr_At ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size > 0 then
      Value := new String_Arr(0..Integer(Size-1));
      for I in Value.all'Range loop
        Self.ReadString( Value(I) );
      end loop;
    else
      Value := null;
    end if;
  end;

  procedure ReadStrings( Self : in out ByteBuffer_Class; Value : out String_Arr ) is
    Size : Int32;
  begin
    Self.ReadInt( Size );
    if Size /= Value'Length then
      raise Illegal_Array_Length;
    end if;
    for I in Value'Range loop
      Self.ReadString( Value(I) );
    end loop;
  end;

  function ToByteArr( Value : String ) return Byte_Arr is
    arr : Byte_Arr(0..3);
  begin
    arr(0) := ToByte(Value(Value'First));
    arr(1) := ToByte(Value(Value'First+1));
    arr(2) := ToByte(Value(Value'First+2));
    arr(3) := ToByte(Value(Value'First+3));
    return arr;
  end;

  versionHigh : constant Byte := 0;
  versionLow : constant Byte := 5;
  protocolID : constant Byte_Arr(0..3) := ToByteArr("opsp");

  procedure WriteProtocol( Self : in out ByteBuffer_Class ) is
  begin
    Self.WriteChars( protocolID );
    Self.WriteChar( versionLow );
    Self.WriteChar( versionHigh );
  end;

  procedure CheckProtocol( Self : in out ByteBuffer_Class; Result : out Boolean ) is
    arr : Byte_Arr(0..3);
    InVersionHigh, InVersionLow : Byte;
  begin
    Result := True;

    Self.ReadChars( arr );
    Self.ReadChar( InVersionLow );
    Self.ReadChar( InVersionHigh );

    if arr /= protocolID then
      Result := False;
    end if;

    if (InVersionHigh /= versionHigh) or (InVersionLow /= versionLow) then
      Result := False;
    end if;
  end;

  procedure InitInstance( Self : in out ByteBuffer_Class; mMap : MemoryMap_Class_At ) is
  begin
    Self.MemMap := mMap;
    Self.Index  := 0;
    Self.TotalSize := 0;
    Self.CurrentSegment := 0;
    Self.NextSegmentAt := Self.MemMap.SegmentSize;
  end InitInstance;

  overriding procedure Finalize( Self : in out ByteBuffer_Class ) is
  begin
    null;
  end Finalize;

end Ops_Pa.ByteBuffer_Pa;
