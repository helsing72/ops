unit uOps.XMLArchiverOut;

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

uses System.Generics.Collections,
     System.SyncObjs,
     SysUtils,
     Xml.XmlDoc,
     Xml.XmlDom,
     Xml.XmlIntf,
     uOps.Types,
     uOPs.Error,
     uOps.ArchiverInOut;

type
  TXMLArchiverOut = class(TArchiverInOut)
  private
    FFmt : TFormatSettings;
    FXmlString : string;
    FCurrentTabDepth : Integer;
    FTopNode : string;

    procedure Add(xml : string);
    function Tab : string;

  public
    constructor Create(topNode : string);
    destructor Destroy; override;

    function isOut : Boolean; override;

    procedure Close;

    procedure inout(const name : String; var value : Boolean); overload; override;
    procedure inout(const name : String; var value : Byte); overload; override;
    procedure inout(const name : String; var value : Int32); overload; override;
    procedure inout(const name : String; var value : Int16); overload; override;
    procedure inout(const name : String; var value : Int64); overload; override;
    procedure inout(const name : String; var value : Single); overload; override;
    procedure inout(const name : String; var value : Double); overload; override;
    procedure inout(const name : String; var value : AnsiString); overload; override;
    procedure inout(const name : String; var value : TSerializable); overload; override;
    procedure inout(const name : String; var value : TSerializable; element : Integer); overload; override;

		procedure inout(const name : String; buffer : PByte; bufferSize : Integer); overload; override;

		function inout2(const name : String; var value : TSerializable) : TSerializable; overload; override;

    function inout2(const name : String; var value : TSerializable; element : Integer) : TSerializable; overload; override;

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

    property XmlString : string read FXmlString;

    function beginList(const name : String; size : Integer) : Integer; override;
    procedure endList(const name : String); override;

  end;

implementation

uses
  uOps.OPSObject,
  uOps.Exceptions;

const
  cEndl = #13#10;

function StartTag(name : string) : string; overload;
begin
  Result := '<' + name + '>';
end;

function StartTag(name : string; attribute : string) : string; overload;
begin
  Result := '<' + name + ' ' + attribute + '>';
end;

function EndTag(name : string) : string;
begin
  Result := '</' + name + '>';
end;

function TXMLArchiverOut.isOut : Boolean;
begin
  Result := True;
end;

procedure TXMLArchiverOut.Add(xml : string);
begin
  FXmlString := FXmlString + Tab + xml + cEndl;
end;

function TXMLArchiverOut.Tab : string;
var
  i : Integer;
begin
  Result := '';
  for i := 0 to FCurrentTabDepth-1 do begin
    Result := Result + '   ';
  end;
end;

constructor TXMLArchiverOut.Create(topNode : string);
begin
  inherited Create;
  FTopNode := topNode;
  FFmt := TFormatSettings.Create;
  FFmt.DecimalSeparator := '.';
  Add(StartTag(topNode));
  Inc(FCurrentTabDepth);
end;

destructor TXMLArchiverOut.Destroy;
begin
  inherited;
end;

procedure TXMLArchiverOut.Close;
begin
  Dec(FCurrentTabDepth);
  Add(EndTag(FTopNode));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : Boolean);
const
  cBool : array[Boolean] of string = ('false', 'true');
begin
  Add(StartTag(name) + cBool[value] + EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : Byte);
begin
  Add(StartTag(name) + IntToStr(value) + EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : Int32);
begin
  Add(StartTag(name) + IntToStr(value) + EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : Int16);
begin
  Add(StartTag(name) + IntToStr(value) + EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : Int64);
begin
  Add(StartTag(name) + IntToStr(value) + EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : Single);
begin
  Add(StartTag(name) + FloatToStr(value, FFmt) + EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : Double);
begin
  Add(StartTag(name) + FloatToStr(value, FFmt) + EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : AnsiString);
begin
  Add(StartTag(name) + string(value) + EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TSerializable);
var
  Obj : TOPSObject;
begin
  if value is TOPSObject then begin
    Obj := value as TOPSObject;
    Add(StartTag(name, 'type = "' + string(Obj.TypesString) + '"'));
    Inc(FCurrentTabDepth);
    value.Serialize(Self);
    Dec(FCurrentTabDepth);
    Add(EndTag(name));
  end;
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TSerializable; element : Integer);
var
  Obj : TOPSObject;
begin
  if value is TOPSObject then begin
    Obj := value as TOPSObject;
    Add(StartTag(name, 'type = "' + string(Obj.TypesString) + '"'));
    Inc(FCurrentTabDepth);
    value.Serialize(Self);
    Dec(FCurrentTabDepth);
    Add(EndTag(name));
  end;
end;

procedure TXMLArchiverOut.inout(const name : String; buffer : PByte; bufferSize : Integer);
begin
  ///TODO
end;

function TXMLArchiverOut.inout2(const name : String; var value : TSerializable) : TSerializable;
var
  Obj : TOPSObject;
begin
  if value is TOPSObject then begin
    Obj := value as TOPSObject;
    Add(StartTag(name, 'type = "' + string(Obj.TypesString) + '"'));
    Inc(FCurrentTabDepth);
    value.Serialize(Self);
    Dec(FCurrentTabDepth);
    Add(EndTag(name));
  end;
  Result := value;
end;

function TXMLArchiverOut.inout2(const name : String; var value : TSerializable; element : Integer) : TSerializable;
var
  Obj : TOPSObject;
begin
  if value is TOPSObject then begin
    Obj := value as TOPSObject;
    Add(StartTag(name, 'type = "' + string(Obj.TypesString) + '"'));
    Inc(FCurrentTabDepth);
    value.Serialize(Self);
    Dec(FCurrentTabDepth);
    Add(EndTag(name));
  end;
  Result := value;
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TDynBooleanArray);
var
  i : Integer;
begin
  Add(StartTag(name));
  for i := 0 to Length(value) - 1 do begin
    Inc(FCurrentTabDepth);
    inout('element', value[i]);
    Dec(FCurrentTabDepth);
  end;
  Add(EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TDynByteArray);
var
  i : Integer;
begin
  Add(StartTag(name));
  for i := 0 to Length(value) - 1 do begin
    Inc(FCurrentTabDepth);
    inout('element', value[i]);
    Dec(FCurrentTabDepth);
  end;
  Add(EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TDynInt32Array);
var
  i : Integer;
begin
  Add(StartTag(name));
  for i := 0 to Length(value) - 1 do begin
    Inc(FCurrentTabDepth);
    inout('element', value[i]);
    Dec(FCurrentTabDepth);
  end;
  Add(EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TDynInt16Array);
var
  i : Integer;
begin
  Add(StartTag(name));
  for i := 0 to Length(value) - 1 do begin
    Inc(FCurrentTabDepth);
    inout('element', value[i]);
    Dec(FCurrentTabDepth);
  end;
  Add(EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TDynInt64Array);
var
  i : Integer;
begin
  Add(StartTag(name));
  for i := 0 to Length(value) - 1 do begin
    Inc(FCurrentTabDepth);
    inout('element', value[i]);
    Dec(FCurrentTabDepth);
  end;
  Add(EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TDynSingleArray);
var
  i : Integer;
begin
  Add(StartTag(name));
  for i := 0 to Length(value) - 1 do begin
    Inc(FCurrentTabDepth);
    inout('element', value[i]);
    Dec(FCurrentTabDepth);
  end;
  Add(EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TDynDoubleArray);
var
  i : Integer;
begin
  Add(StartTag(name));
  for i := 0 to Length(value) - 1 do begin
    Inc(FCurrentTabDepth);
    inout('element', value[i]);
    Dec(FCurrentTabDepth);
  end;
  Add(EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : String; var value : TDynAnsiStringArray);
var
  i : Integer;
begin
  Add(StartTag(name));
  for i := 0 to Length(value) - 1 do begin
    Inc(FCurrentTabDepth);
    inout('element', value[i]);
    Dec(FCurrentTabDepth);
  end;
  Add(EndTag(name));
end;

procedure TXMLArchiverOut.inout(const name : string; var value : TDynSerializableArray);
var
  i : Integer;
begin
  Add(StartTag(name));
  for i := 0 to Length(value) - 1 do begin
    Inc(FCurrentTabDepth);
    inout('element', value[i]);
    Dec(FCurrentTabDepth);
  end;
  Add(EndTag(name));
end;

procedure TXMLArchiverOut.inoutfixarr(const name : string; value : Pointer; numElements : Integer; totalSize : Integer);
begin
  raise EArchiverException.Create('TXMLArchiverOut.inoutfixarr NYI');
end;

procedure TXMLArchiverOut.inoutfixarr(const name : string; var value : array of AnsiString; numElements : Integer);
begin
  raise EArchiverException.Create('TXMLArchiverOut.inoutfixarr NYI');
end;

function TXMLArchiverOut.beginList(const name : String; size : Integer) : Integer;
begin
  Add(StartTag(name));
  Inc(FCurrentTabDepth);
  Result := size;
end;

procedure TXMLArchiverOut.endList(const name : String);
begin
  Dec(FCurrentTabDepth);
  Add(EndTag(name));
end;

end.

