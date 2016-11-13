unit uOps.ByteBuffer;

(**
*
* Copyright (C) 2016 Lennart Andersson.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.
*
* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*)

interface

uses uOps.Types,
     uOps.MemoryMap;

type
  TByteBuffer = class(TObject)
  private
    ///Buffer used to store data to be written or read.
    FMemMap : TMemoryMap;

    ///index pointing out where in the buffer to do the next read or write.
    ///This variable is automatically incremented by the read and write  operations.
    FIndex : UInt32;

		FTotalSize : UInt32;
		FNextSegmentAt : UInt32;
		FCurrentSegment : UInt32;

  public
    ///Writes the length first bytes from chars to the buffer and increments index by length.
    procedure WriteChars(chars : PByte; length : Integer);
    ///Reads length number of bytes to chars and increments index by length.
    procedure ReadChars(chars : PByte; length : Integer);

		procedure WriteNewSegment;

  private
		procedure ReadNewSegment;
		procedure WriteBytes(var buf : array of Byte; offset : Integer; length : Integer); overload;
		procedure ReadBytes(var buf : array of Byte; offset : Integer; length : Integer); overload;

  public
    constructor Create(mMap : TMemoryMap);
    destructor Destroy; override;

    ///Only valid for a ByteBuffer instance used to write data.
    ///Returns the the number of bytes containing valid data in the buffer so far.
    function GetSize : Integer;

		///Resets the whole buffer to creation state
		procedure Reset;

    ///Resets the internal offset pointer to 0 (zero)
    procedure ResetIndex;

		function GetNrOfSegments : UInt32;
		function GetSegmentSize(i : UInt32) : UInt32;
		function GetSegment(i : UInt32) : PByte;
		procedure Finish;

    ///Writes the 4 bytes making up f to the buffer and increments index by 4.
    ///Byte order is swaped before writing takes place.
    procedure WriteFloat(f : Single);
    ///Writes the 2 bytes making up i to the buffer and increments index by 2.
    ///Byte order is swaped before writing takes place.
    procedure WriteShort(i : Int16);
    ///Writes the 4 bytes making up i to the buffer and increments index by 4.
    ///Byte order is swaped before writing takes place.
    procedure WriteInt(i : Int32);
    ///Writes the 8 bytes making up l to the buffer and increments index by 8.
    ///Byte order is swaped before writing takes place.
    procedure WriteLong(l : Int64);
    ///Writes the 8 bytes making up d to the buffer and increments index by 4.
    ///Byte order is swaped before writing takes place.
    procedure WriteDouble(d : Double);
    ///Writes c to the buffer and increments index by 1.
    procedure WriteChar(c : Byte);
    ///Writes flag to the buffer and increments index by 1.
    procedure WriteBoolean(flag : Boolean);
    ///Writes Length(s) followed by s to the buffer as a c-string (8-bit chars) and increments the buffer by Length(s) + 4.
    procedure WriteString(s : AnsiString);

    ///Reads 4 bytes from the buffer and returns them as a float. Index is increased by 4.
    ///Byte order is swaped before return.
    function ReadFloat : Single;
    ///Reads 2 bytes from the buffer and returns them as an short. Index is increased by 2.
    ///Byte order is swaped before return.
    function ReadShort : Integer;
    ///Reads 4 bytes from the buffer and returns them as an int. Index is increased by 4.
    ///Byte order is swaped before return.
    function ReadInt : Integer;
    ///Reads 8 bytes from the buffer and returns them as a long long (__int64). Index is increased by 8.
    ///Byte order is swaped before return.
    function ReadLong : Int64;
    ///Reads 8 bytes from the buffer and returns them as a double. Index is increased by 8.
    ///Byte order is swaped before return.
    function ReadDouble : Double;
    ///Reads 1 byte from the buffer and returns it as a char. Index is increased by 1.
    function ReadChar : Byte;
    ///Reads 1 byte from the buffer and returns it as a boolean. Index is increased by 1.
    function ReadBoolean: Boolean;
    ///Reads an int (length) from the buffer followed by length number of chars returned as a string. Index is increased by length + 4.
    function ReadString : AnsiString;

    /////////////////////

    procedure WriteFloats(value : TDynSingleArray);
    procedure WriteShorts(value : TDynInt16Array);
    procedure WriteInts(value : TDynInt32Array);
    procedure WriteLongs(value : TDynInt64Array);
    procedure WriteDoubles(value : TDynDoubleArray);
    procedure WriteBytes(value : TDynByteArray); overload;
    procedure WriteBooleans(value : TDynBooleanArray);
    procedure WriteStrings(value : TDynAnsiStringArray);

    function ReadFloats : TDynSingleArray;
    function ReadShorts : TDynInt16Array;
    function ReadInts : TDynInt32Array;
    function ReadLongs : TDynInt64Array;
    function ReadDoubles : TDynDoubleArray;
    function ReadBytes : TDynByteArray; overload;
    function ReadBooleans : TDynBooleanArray;
    function ReadStrings : TDynAnsiStringArray;

    /////////////////////

		procedure WriteProtocol;
		function CheckProtocol : Boolean;
  end;

implementation

{ TByteBuffer }

constructor TByteBuffer.Create(mMap : TMemoryMap);
begin
  FMemMap := mMap;
  FIndex  := 0;
  FTotalSize := 0;
  FCurrentSegment := 0;
  FNextSegmentAt := fMemMap.SegmentSize;
end;

destructor TByteBuffer.Destroy;
begin
  inherited;
end;

//Resets the whole buffer to creation state
procedure TByteBuffer.Reset();
begin
  FIndex  := 0;
  FTotalSize := 0;
  FCurrentSegment := 0;
  FNextSegmentAt := fMemMap.SegmentSize;
end;

function TByteBuffer.GetNrOfSegments : UInt32;
begin
  Result := FCurrentSegment + 1;
end;

function TByteBuffer.GetSegmentSize(i : UInt32) : UInt32;
begin
  if (i < FCurrentSegment) then begin
    Result := FMemMap.SegmentSize;
  end else begin
    Result := FIndex;
  end;
end;

function TByteBuffer.GetSegment(i : UInt32) : PByte;
begin
  Result := FMemMap.GetSegment(i);
end;

procedure TByteBuffer.Finish;
var
  i, oldIndex, oldNextSegmentAt, nrSeg : UInt32;
begin
  oldIndex := FIndex;
  oldNextSegmentAt := FNextSegmentAt;
  FNextSegmentAt := 0;
  nrSeg := GetNrOfSegments;
  for i := 0 to nrSeg - 1 do begin
    FIndex := 6;
    FCurrentSegment := i;
    FNextSegmentAt := FNextSegmentAt + FMemMap.SegmentSize;
    WriteInt(nrSeg);
  end;
  FIndex := oldIndex;
  FNextSegmentAt := oldNextSegmentAt;
end;

procedure TByteBuffer.WriteChars(chars: PByte; length: Integer);
var
  bytesLeftInSegment : Integer;
begin
  bytesLeftInSegment := FMemMap.SegmentSize - FIndex;
  if bytesLeftInSegment >= length then begin
    System.Move(chars^, PByte(FMemMap.GetSegment(FCurrentSegment) + FIndex)^, length);
    Inc(FIndex, length);
    Inc(FTotalSize, length);
  end else begin
    System.Move(chars^, PByte(FMemMap.GetSegment(FCurrentSegment) + FIndex)^, bytesLeftInSegment);
    Inc(FIndex, bytesLeftInSegment);
    Inc(FTotalSize, bytesLeftInSegment);
    FNextSegmentAt := FNextSegmentAt + FMemMap.SegmentSize;
    Inc(FCurrentSegment);
    WriteNewSegment();
    WriteChars(chars + bytesLeftInSegment, length - bytesLeftInSegment);
  end;
end;

procedure TByteBuffer.WriteNewSegment;
var
  tInt : Integer;
begin
  FIndex := 0;
  WriteProtocol();
  tInt := 0;
  WriteInt(tInt);
  WriteInt(FCurrentSegment);
end;

procedure TByteBuffer.ReadNewSegment;
begin
  FIndex := 0;
  FNextSegmentAt := FNextSegmentAt + FMemMap.SegmentSize;
  CheckProtocol;
  ReadInt;
  ReadInt;
end;

procedure TByteBuffer.ReadChars(chars: PByte; length: Integer);
var
  bytesLeftInSegment : Integer;
begin
  bytesLeftInSegment := FMemMap.SegmentSize - FIndex;
  if bytesLeftInSegment >= length then begin
    System.Move(PByte(FMemMap.GetSegment(FCurrentSegment) + FIndex)^, chars^, length);
    Inc(FIndex, length);
    Inc(FTotalSize, length);
  end else begin
    System.Move(PByte(FMemMap.GetSegment(FCurrentSegment) + FIndex)^, chars^, bytesLeftInSegment);
    Inc(FIndex, bytesLeftInSegment);
    Inc(FCurrentSegment);
    Inc(FTotalSize, bytesLeftInSegment);
    ReadNewSegment;
    ReadChars(chars + bytesLeftInSegment, length - bytesLeftInSegment);
  end;
end;

function TByteBuffer.GetSize: Integer;
begin
  Result := FTotalSize;
end;

procedure TByteBuffer.WriteChar(c: Byte);
begin
  WriteChars(PByte(@c), 1);
end;

function TByteBuffer.ReadChar: Byte;
begin
  ReadChars(PByte(@Result), 1);
end;

procedure TByteBuffer.WriteBoolean(flag: Boolean);
const
  Bool_C : array[Boolean] of Byte = (0, 1);
begin
  WriteChar(Bool_C[flag]);
end;

function TByteBuffer.ReadBoolean: Boolean;
begin
  Result := ReadChar <> 0;
end;

procedure TByteBuffer.WriteDouble(d: Double);
begin
  WriteChars(PByte(@d), 8);
end;

function TByteBuffer.ReadDouble: Double;
begin
  ReadChars(PByte(@Result), 8);
end;

procedure TByteBuffer.WriteFloat(f: Single);
begin
  WriteChars(PByte(@f), 4);
end;

function TByteBuffer.ReadFloat: Single;
begin
  ReadChars(PByte(@Result), 4);
end;

procedure TByteBuffer.WriteShort(i: Int16);
begin
  WriteChars(PByte(@i), 2);
end;

function TByteBuffer.ReadShort: Integer;
begin
  ReadChars(PByte(@Result), 2);
end;

procedure TByteBuffer.WriteInt(i: Int32);
begin
  WriteChars(PByte(@i), 4);
end;

function TByteBuffer.ReadInt: Integer;
begin
  ReadChars(PByte(@Result), 4);
end;

procedure TByteBuffer.WriteLong(l: Int64);
begin
  WriteChars(PByte(@l), 8);
end;

function TByteBuffer.ReadLong: Int64;
begin
  ReadChars(PByte(@Result), 8);
end;

procedure TByteBuffer.WriteString(s: AnsiString);
begin
  WriteInt(Length(s));
  WriteChars(PByte(s), Length(s));
end;

function TByteBuffer.ReadString: AnsiString;
var
  Len : Integer;
begin
  Len := ReadInt;
  SetLength(Result, Len);
  ReadChars(PByte(Result), Len);
end;

// ------------------

procedure TByteBuffer.WriteFloats(value : TDynSingleArray);
var
  size : Integer;
begin
  size := Length(value);
  WriteInt(size);
  if (size > 0) then WriteChars(PByte(@value[0]), size * 4);
end;

function TByteBuffer.ReadFloats : TDynSingleArray;
var
  size : Integer;
begin
  size := ReadInt;
  SetLength(Result, size);
  if size > 0 then ReadChars(PByte(@Result[0]), size * 4);
end;

procedure TByteBuffer.WriteShorts(value : TDynInt16Array);
var
  size : Integer;
begin
  size := Length(value);
  WriteInt(size);
  if size > 0 then WriteChars(PByte(@value[0]), size * 2);
end;

function TByteBuffer.ReadShorts : TDynInt16Array;
var
  size : Integer;
begin
  size := ReadInt;
  SetLength(Result, size);
  if size > 0 then ReadChars(PByte(@Result[0]), size * 2);
end;

procedure TByteBuffer.WriteInts(value : TDynInt32Array);
var
  size : Integer;
begin
  size := Length(value);
  WriteInt(size);
  if size > 0 then WriteChars(PByte(@value[0]), size * 4);
end;

function TByteBuffer.ReadInts : TDynInt32Array;
var
  size : Integer;
begin
  size := ReadInt;
  SetLength(Result, size);
  if size > 0 then ReadChars(PByte(@Result[0]), size * 4);
end;

procedure TByteBuffer.WriteLongs(value : TDynInt64Array);
var
  size : Integer;
begin
  size := Length(value);
  WriteInt(size);
  if size > 0 then WriteChars(PByte(@value[0]), size * 8);
end;

function TByteBuffer.ReadLongs : TDynInt64Array;
var
  size : Integer;
begin
  size := ReadInt;
  SetLength(Result, size);
  if size > 0 then ReadChars(PByte(@Result[0]), size * 8);
end;

procedure TByteBuffer.WriteDoubles(value : TDynDoubleArray);
var
  size : Integer;
begin
  size := Length(value);
  WriteInt(size);
  if size > 0 then WriteChars(PByte(@value[0]), size * 8);
end;

function TByteBuffer.ReadDoubles : TDynDoubleArray;
var
  size : Integer;
begin
  size := ReadInt;
  SetLength(Result, size);
  if size > 0 then ReadChars(PByte(@Result[0]), size * 8);
end;

procedure TByteBuffer.WriteBytes(value : TDynByteArray);
var
  size : Integer;
begin
  size := Length(value);
  WriteInt(size);
  WriteBytes(value, 0, size);
end;

function TByteBuffer.ReadBytes : TDynByteArray;
var
  size : Integer;
begin
  size := ReadInt;
  SetLength(Result, size);
  ReadBytes(Result, 0, size);
end;

procedure TByteBuffer.WriteBytes(var buf : array of Byte; offset : Integer; length : Integer);
begin
  WriteChars(PByte(@buf[offset]), length);
end;

procedure TByteBuffer.ReadBytes(var buf : array of Byte; offset : Integer; length : Integer);
begin
  ReadChars(PByte(@buf[offset]), length);
end;

procedure TByteBuffer.WriteBooleans(value : TDynBooleanArray);
const
  Bool_C : array[Boolean] of Byte = (0, 1);
var
  i, size : Integer;
begin
  size := Length(value);
  WriteInt(size);
  for i := 0 to size - 1 do begin
    WriteChar(Bool_C[value[i]]);
  end;
end;

function TByteBuffer.ReadBooleans : TDynBooleanArray;
var
  i, size : Integer;
begin
  size := ReadInt;
  SetLength(Result, size);
  for i := 0 to size - 1 do begin
    Result[i] := ReadChar > 0;
  end;
end;

procedure TByteBuffer.WriteStrings(value : TDynAnsiStringArray);
var
  i, size : Integer;
begin
  size := Length(value);
  WriteInt(size);
  for i := 0 to size -1 do begin
    WriteString(value[i]);
  end;
end;

function TByteBuffer.ReadStrings : TDynAnsiStringArray;
var
  i, size : Integer;
begin
  size := ReadInt;
  SetLength(Result, size);
  for i := 0 to size - 1 do begin
    Result[i] := ReadString;
  end;
end;

const
  versionHigh = 0;
  versionLow = 5;
  protocolID : AnsiString = 'opsp';

procedure TByteBuffer.WriteProtocol;
var
  i : Integer;
  Buffer : array [0..3] of AnsiChar;
begin
  for i := 0 to 3 do Buffer[i] := protocolID[i+1];
  WriteChars(PByte(@Buffer[0]), 4);
  WriteChar(versionLow);
  WriteChar(versionHigh);
end;

function TByteBuffer.CheckProtocol: Boolean;
var
  InVersionHigh, InVersionLow : Byte;
  InProtocolIDChars : array[0..4] of AnsiChar;
begin
  Result := True;

  ReadChars(PByte(@InProtocolIDChars[0]), 4);
  InProtocolIDChars[4] := #0;

  if (InProtocolIdChars <> protocolID) then begin
    Result := False;
  end;

  InVersionLow := ReadChar;
  InVersionHigh := ReadChar;
  if( (InVersionHigh <> versionHigh) or (InVersionLow <> versionLow) ) then begin
    Result := False;
  end;
end;

procedure TByteBuffer.ResetIndex;
begin
  FIndex := 0;
end;

end.

