unit uOps.MemoryMap;

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

uses SysUtils;

type
  TPByteArray = array[0..65535] of PByte;
  PPByte = ^TPByteArray;

  TMemoryMap = class(TObject)
  private
    FWidth : UInt32;
    FHeight : UInt32;
    FDataCreator : Boolean;
    FBytes : PPByte;

    function GetTotalSize : UInt32;

  public
    constructor Create(Width : UInt32; Height : UInt32); overload;
    constructor Create(Segment : PByte; Size : UInt32); overload;
    destructor Destroy; override;

    // Only valid to call for a memorymap created with a buffer and size
    procedure ChangeBuffer(Segment : PByte; Size : UInt32);

    function GetSegment(i : UInt32) : PByte;

    procedure CopyToBytes(Dest : PByte; StartIndex : UInt32; EndIndex : UInt32);

    property SegmentSize : UInt32 read FHeight;
    property NrOfSegments : UInt32 read FWidth;
    property TotalSize : UInt32 read GetTotalSize;
  end;

implementation

constructor TMemoryMap.Create(Width : UInt32; Height : UInt32);
var
  i : UInt32;
begin
  FWidth := Width;
  FHeight := Height;
  FDataCreator := True;

  // Allocate segment pointer array
  GetMem(FBytes, SizeOf(PByte) * FWidth);
  // Allocate all segments in one chunk
  GetMem(FBytes[0], FWidth * FHeight);
  // Setup segment pointers to point into the allocated chunk
  for i := 1 to fWidth - 1 do begin
    FBytes[i] := FBytes[i-1] + FHeight;
  end;
end;

constructor TMemoryMap.Create(Segment : PByte; Size : UInt32);
begin
  FWidth := 1;
  FHeight := Size;
  FDataCreator := False;

  // Allocate segment pointer array
  GetMem(FBytes, SizeOf(PByte) * FWidth);
  FBytes[0] := Segment;
end;

destructor TMemoryMap.Destroy;
begin
  // Delete all data.
  if FDataCreator then begin
		FreeMem(FBytes[0]);
	end;
	FreeMem(FBytes);
  inherited;
end;

// Only valid to call for a memorymap created with a buffer and size
procedure TMemoryMap.ChangeBuffer(Segment : PByte; Size : UInt32);
begin
  if FDataCreator then Exit;  // Can't be changed if we own the buffers
  FHeight := Size;
  FBytes[0] := Segment;
end;

function TMemoryMap.GetTotalSize : UInt32;
begin
  Result := FHeight * FWidth;
end;

function TMemoryMap.GetSegment(i : UInt32) : PByte;
begin
  Result := nil;
  if i < FWidth then Result := FBytes[i];
end;

///Makes a copy of the content of this memory to dest. startIndex and endIndex are memory map relative.
procedure TMemoryMap.CopyToBytes(Dest : PByte; StartIndex : UInt32; EndIndex : UInt32);
var
  BytesToCopy : Int64;
begin
  BytesToCopy := EndIndex - StartIndex + 1;
  if BytesToCopy > GetTotalSize then BytesToCopy := GetTotalSize;
  if BytesToCopy < 0 then BytesToCopy := 0;
  // Since all segments are in one chunk after each other we can do a simple memcpy()
  System.Move(PByte(FBytes[0] + StartIndex)^, Dest^, BytesToCopy);
end;

end.

