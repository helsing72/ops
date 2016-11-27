unit uOps.PrintArchiverOut;

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

uses System.Generics.Collections,
     System.SyncObjs,
     SysUtils,
     uOps.Types,
     uOPs.Error,
     uOps.ArchiverInOut;

type
  TPrintArchiverOut = class(TArchiverInOut)
  private
    FFmt : TFormatSettings;
    FCurrentTabDepth : Integer;
    FPrintString : string;

    procedure Add(str : string);
    function Tab : string;

  public
    constructor Create;
    destructor Destroy; override;

    function isOut : Boolean; override;

    procedure Close;

    procedure printObject(name : string; obj : TSerializable);

    procedure inout(const name : String; var value : Boolean); overload; override;
    procedure inout(const name : String; var value : Byte); overload; override;
    procedure inout(const name : String; var value : Int32); overload; override;
    procedure inout(const name : String; var value : Int16); overload; override;
    procedure inout(const name : String; var value : Int64); overload; override;
    procedure inout(const name : String; var value : Single); overload; override;
    procedure inout(const name : String; var value : Double); overload; override;
    procedure inout(const name : String; var value : AnsiString); overload; override;
    procedure inout(const name : String; var value : TSerializable); overload; override;

		procedure inout(const name : String; buffer : PByte; bufferSize : Integer); overload; override;

		function inout2(const name : String; var value : TSerializable) : TSerializable; overload; override;

    function inout(const name : String; var value : TSerializable; element : Integer) : TSerializable; overload; override;

    procedure inout(const name : String; var value : TDynBooleanArray); overload; override;
    procedure inout(const name : String; var value : TDynByteArray); overload; override;
    procedure inout(const name : String; var value : TDynInt32Array); overload; override;
    procedure inout(const name : String; var value : TDynInt16Array); overload; override;
    procedure inout(const name : String; var value : TDynInt64Array); overload; override;
    procedure inout(const name : String; var value : TDynSingleArray); overload; override;
    procedure inout(const name : String; var value : TDynDoubleArray); overload; override;
    procedure inout(const name : String; var value : TDynAnsiStringArray); overload; override;

    procedure inoutfixarr(const name : string; value : Pointer; numElements : Integer; totalSize : Integer); overload; override;
    procedure inoutfixarr(const name : string; var value : array of AnsiString; numElements : Integer); overload; override;

    procedure inout(const name : string; var value : TDynSerializableArray); overload; override;

    property PrintString : string read FPrintString;

  protected
    function beginList(const name : String; size : Integer) : Integer; override;
    procedure endList(const name : String); override;
  end;

implementation

uses uOps.OPSObject,
     uOps.Exceptions;

const
  cEndl = #13#10;

procedure TPrintArchiverOut.Add(str : string);
begin
  FPrintString := FPrintString + Tab + str + cEndl;
end;

function TPrintArchiverOut.Tab : string;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to FCurrentTabDepth-1 do begin
    Result := Result + '   ';
  end;
end;

constructor TPrintArchiverOut.Create;
begin
  inherited Create;
  FFmt := TFormatSettings.Create;
  FFmt.DecimalSeparator := '.';
end;

destructor TPrintArchiverOut.Destroy;
begin
  inherited;
end;

function TPrintArchiverOut.isOut : Boolean;
begin
  Result := True;
end;

procedure TPrintArchiverOut.Close;
begin
end;

procedure TPrintArchiverOut.printObject(name : string; obj : TSerializable);
begin
  Add(cEndl + '________________Begin Object___________________' + cEndl + cEndl);
  inout(name, obj);
  Add(cEndl + '_________________End Object____________________' + cEndl);
end;

procedure TPrintArchiverOut.inout(const name : String; var value : Boolean);
const
  cBool : array[Boolean] of string = ('False', 'True');
begin
  Add(name + ' = ' + cBool[value]);
end;

procedure TPrintArchiverOut.inout(const name : String; var value : Byte);
begin
  Add(name + ' = ' + IntToStr(value));
end;

procedure TPrintArchiverOut.inout(const name : String; var value : Int32);
begin
  Add(name + ' = ' + IntToStr(value));
end;

procedure TPrintArchiverOut.inout(const name : String; var value : Int16);
begin
  Add(name + ' = ' + IntToStr(value));
end;

procedure TPrintArchiverOut.inout(const name : String; var value : Int64);
begin
  Add(name + ' = ' + IntToStr(value));
end;

procedure TPrintArchiverOut.inout(const name : String; var value : Single);
begin
  Add(name + ' = ' + FloatToStr(value));
end;

procedure TPrintArchiverOut.inout(const name : String; var value : Double);
begin
  Add(name + ' = ' + FloatToStr(value));
end;

procedure TPrintArchiverOut.inout(const name : String; var value : AnsiString);
begin
  Add(name + ' = ' + string(value));
end;

procedure TPrintArchiverOut.inout(const name : String; var value : TSerializable);
var
  Obj : TOPSObject;
begin
  if value is TOPSObject then begin
    Obj := value as TOPSObject;
    Add(name + ' type = "' + string(Obj.TypesString) + '"');
    Inc(FCurrentTabDepth);
    value.Serialize(Self);
    Dec(FCurrentTabDepth);
  end;
end;

procedure TPrintArchiverOut.inout(const name : String; buffer : PByte; bufferSize : Integer);
begin
  Add(name + ' = ' + ' Byte buffer NYI ');
  ///TODO
end;

function TPrintArchiverOut.inout2(const name : String; var value : TSerializable) : TSerializable;
var
  Obj : TOPSObject;
begin
  if value is TOPSObject then begin
    Obj := value as TOPSObject;
    Add(name + ' type = "' + string(Obj.TypesString) + '"');
    Inc(FCurrentTabDepth);
    value.Serialize(Self);
    Dec(FCurrentTabDepth);
  end;
  Result := value;
end;

function TPrintArchiverOut.inout(const name : String; var value : TSerializable; element : Integer) : TSerializable;
var
  Obj : TOPSObject;
begin
  if value is TOPSObject then begin
    Obj := value as TOPSObject;
    Add(name + ' type = "' + string(Obj.TypesString) + '"');
    Inc(FCurrentTabDepth);
    value.Serialize(Self);
    Dec(FCurrentTabDepth);
  end;
  Result := value;
end;

procedure TPrintArchiverOut.inout(const name : String; var value : TDynBooleanArray);
const
  cBool : array[Boolean] of string = ('False', 'True');
var
  val0, valx : string;
begin
  if Length(value) > 0 then begin
    val0 := cBool[value[0]];
    valx := cBool[value[High(value)]];
  end;
  Add(name + '(size = ' + IntToStr(Length(value)) + ') = [' + val0 + '...' + valx + ']');
end;

procedure TPrintArchiverOut.inout(const name : String; var value : TDynByteArray);
var
  val0, valx : string;
begin
  if Length(value) > 0 then begin
    val0 := IntToStr(value[0]);
    valx := IntToStr(value[High(value)]);
  end;
  Add(name + '(size = ' + IntToStr(Length(value)) + ') = [' + val0 + '...' + valx + ']');
end;

procedure TPrintArchiverOut.inout(const name : String; var value : TDynInt32Array);
var
  val0, valx : string;
begin
  if Length(value) > 0 then begin
    val0 := IntToStr(value[0]);
    valx := IntToStr(value[High(value)]);
  end;
  Add(name + '(size = ' + IntToStr(Length(value)) + ') = [' + val0 + '...' + valx + ']');
end;

procedure TPrintArchiverOut.inout(const name : String; var value : TDynInt16Array);
var
  val0, valx : string;
begin
  if Length(value) > 0 then begin
    val0 := IntToStr(value[0]);
    valx := IntToStr(value[High(value)]);
  end;
  Add(name + '(size = ' + IntToStr(Length(value)) + ') = [' + val0 + '...' + valx + ']');
end;

procedure TPrintArchiverOut.inout(const name : String; var value : TDynInt64Array);
var
  val0, valx : string;
begin
  if Length(value) > 0 then begin
    val0 := IntToStr(value[0]);
    valx := IntToStr(value[High(value)]);
  end;
  Add(name + '(size = ' + IntToStr(Length(value)) + ') = [' + val0 + '...' + valx + ']');
end;

procedure TPrintArchiverOut.inout(const name : String; var value : TDynSingleArray);
var
  val0, valx : string;
begin
  if Length(value) > 0 then begin
    val0 := FloatToStr(value[0], FFmt);
    valx := FloatToStr(value[High(value)], FFmt);
  end;
  Add(name + '(size = ' + IntToStr(Length(value)) + ') = [' + val0 + '...' + valx + ']');
end;

procedure TPrintArchiverOut.inout(const name : String; var value : TDynDoubleArray);
var
  val0, valx : string;
begin
  if Length(value) > 0 then begin
    val0 := FloatToStr(value[0], FFmt);
    valx := FloatToStr(value[High(value)], FFmt);
  end;
  Add(name + '(size = ' + IntToStr(Length(value)) + ') = [' + val0 + '...' + valx + ']');
end;

procedure TPrintArchiverOut.inout(const name : String; var value : TDynAnsiStringArray);
var
  val0, valx : string;
begin
  if Length(value) > 0 then begin
    val0 := string(value[0]);
    valx := string(value[High(value)]);
  end;
  Add(name + '(size = ' + IntToStr(Length(value)) + ') = [' + val0 + '...' + valx + ']');
end;

procedure TPrintArchiverOut.inout(const name : string; var value : TDynSerializableArray);
var
  i : Integer;
begin
  Add(name + '(size = ' + IntToStr(Length(value)) + ') = ');
  Inc(FCurrentTabDepth);
  for i := 0 to Length(value) - 1 do begin
    inout('element', value[i]);
  end;
  Dec(FCurrentTabDepth);
end;

procedure TPrintArchiverOut.inoutfixarr(const name : string; value : Pointer; numElements : Integer; totalSize : Integer);
begin
  raise EArchiverException.Create('TPrintArchiverOut.inoutfixarr NYI');
end;

procedure TPrintArchiverOut.inoutfixarr(const name : string; var value : array of AnsiString; numElements : Integer);
begin
  raise EArchiverException.Create('TPrintArchiverOut.inoutfixarr NYI');
end;

function TPrintArchiverOut.beginList(const name : String; size : Integer) : Integer;
begin
  Add(name + ' = ');
  Inc(FCurrentTabDepth);
  Result := size;
end;

procedure TPrintArchiverOut.endList(const name : String);
begin
  Dec(FCurrentTabDepth);
end;

end.

