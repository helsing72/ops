unit uOps.ArchiverInOut;

(**
*
* Copyright (C) 2016-2019 Lennart Andersson.
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

uses uOps.Types;

type
  TArchiverInOut = class;

	TSerializable = class(TObject)
  protected
    procedure SetTypesString(types : AnsiString); virtual;
  public
		procedure Serialize(archiver : TArchiverInOut); virtual; abstract;
	end;

  TDynSerializableArray = array of TSerializable;

  TArchiverInOut = class(TObject)
  public
    function isOut : Boolean; virtual; abstract;

    procedure inout(const name : String; var value : Boolean); overload; virtual; abstract;
    procedure inout(const name : String; var value : Byte); overload; virtual; abstract;
    procedure inout(const name : String; var value : Int32); overload; virtual; abstract;
    procedure inout(const name : String; var value : Int16); overload; virtual; abstract;
    procedure inout(const name : String; var value : Int64); overload; virtual; abstract;
    procedure inout(const name : String; var value : Single); overload; virtual; abstract;
    procedure inout(const name : String; var value : Double); overload; virtual; abstract;
    procedure inout(const name : String; var value : AnsiString); overload; virtual; abstract;
    procedure inout(const name : String; var value : TSerializable); overload; virtual; abstract;
    procedure inout(const name : String; var value : TSerializable; element : Integer); overload; virtual; abstract;

		procedure inout(const name : String; buffer : PByte; bufferSize : Integer); overload; virtual; abstract;

		function inout2(const name : String; var value : TSerializable) : TSerializable; overload; virtual; abstract;

    function inout2(const name : String; var value : TSerializable; element : Integer) : TSerializable; overload; virtual; abstract;

    procedure inout(const name : String; var value : TDynBooleanArray); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynByteArray); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynInt32Array); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynInt16Array); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynInt64Array); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynSingleArray); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynDoubleArray); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynAnsiStringArray); overload; virtual; abstract;

    procedure inoutfixarr(const name : string; value : Pointer; numElements : Integer; totalSize : Integer); overload; virtual; abstract;
    procedure inoutfixarr(const name : string; var value : array of AnsiString; numElements : Integer); overload; virtual; abstract;

    procedure inout(const name : string; var Value : TDynSerializableArray); overload; virtual; abstract;

    type
      TSerializableHelper<T: TSerializable, constructor> = class(TObject)
        class procedure inoutfixarr(archiver : TArchiverInOut; const name : string; var value : array of T; numElements : Integer; isAbstract : Boolean);
        class procedure inoutdynarr(archiver : TArchiverInOut; const name : string; var Value : TDynSerializableArray; isAbstract : Boolean);
      end;

    function beginList(const name : String; size : Integer) : Integer; virtual; abstract;
    procedure endList(const name : String); virtual; abstract;

  protected
    procedure SetTypesString(obj : TSerializable; types : AnsiString);
  end;

implementation

uses SysUtils,
     uOps.Exceptions;

procedure TSerializable.SetTypesString(types : AnsiString);
begin
end;

procedure TArchiverInOut.SetTypesString(obj : TSerializable; types : AnsiString);
begin
  obj.SetTypesString(types);
end;

// isAbstract = true --> declared virtual in idl and can therefore be any derived object
class procedure TArchiverInOut.TSerializableHelper<T>.inoutfixarr(
  archiver : TArchiverInOut; const name : string; var value : array of T; numElements : Integer; isAbstract : Boolean);
var
  num, i : Integer;
begin
  num := archiver.beginList(name, numElements);
  if num <> numElements then raise EArchiverException.Create('Illegal size of fix array received. name: ' + name);

  for i := 0 to numElements-1 do begin
    if isAbstract then begin
      // Elements can be of any derived type
      if not archiver.isOut then FreeAndNil(value[i]);
      value[i] := T(archiver.inout2(name, TSerializable(value[i]), i));
    end else begin
      // In this case we know that elements always are of type T
      archiver.inout(name, TSerializable(value[i]), i);
    end;
  end;

  archiver.endList(name);
end;

class procedure TArchiverInOut.TSerializableHelper<T>.inoutdynarr(
  archiver : TArchiverInOut; const name : string; var Value : TDynSerializableArray; isAbstract : Boolean);
var
  size, i : Integer;
begin
  size := archiver.beginList(name, Length(value));

  if archiver.isOut then begin
    // Now loop over all objects in the array
    for i := 0 to size-1 do begin
      if isAbstract then begin
        // Elements can be of any derived type
        archiver.inout2(name, value[i], i);
      end else begin
        // In this case we know that elements always are of type T
        archiver.inout(name, TSerializable(value[i]), i);
      end;
    end;

  end else begin
    // First free ev existing objects in the array
    for i := 0 to Length(value)-1 do begin
      FreeAndNil(value[i]);
    end;

    // Set new length (elements will be nil)
    SetLength(value, size);

    // Now loop over all objects in the array
    for i := 0 to size-1 do begin
      if isAbstract then begin
        value[i] := archiver.inout2(name, value[i]);
      end else begin
        value[i] := T.Create;
        archiver.inout(name, TSerializable(value[i]), i);
      end;
    end;
  end;

  archiver.endList(name);
end;

end.

