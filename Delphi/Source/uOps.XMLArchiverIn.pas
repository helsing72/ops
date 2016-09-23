unit uOps.XMLArchiverIn;

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
     Xml.XmlDoc,
     Xml.XmlDom,
     Xml.XmlIntf,
     uOps.Types,
     uOPs.Error,
     uOps.ArchiverInOut,
     uOps.SerializableInheritingTypeFactory;

type
  TXMLArchiverIn = class(TArchiverInOut)
  private
    FFmt : TFormatSettings;
    FFactory : TSerializableInheritingTypeFactory;
    FDoc : IXMLDocument;
    FCurrentNode: IXMLNode;

    function numElements : Integer;

  public
    constructor Create(xmlString : string; topNode : string; factory : TSerializableInheritingTypeFactory);
    destructor Destroy; override;

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

    procedure inout(const name : string; var value : TDynSerializableArray); overload; override;

  protected
    function beginList(const name : String; size : Integer) : Integer; override;
    procedure endList(const name : String); override;
  end;

implementation

{$IFDEF CONSOLE}
uses
  System.Win.ComObj;
{$ENDIF}

constructor TXMLArchiverIn.Create(xmlString : string; topNode : string; factory : TSerializableInheritingTypeFactory);
begin
{$IFDEF CONSOLE}
  //TCoInitializeExProc = function (pvReserved: Pointer; coInit: Longint): HResult; stdcall;
  //CoInitializeEx: TCoInitializeExProc = nil;
  CoInitializeEx(nil, 0);    // Needed for the TXMLDocument
{$ENDIF}
  FFactory := factory;
  FFmt := TFormatSettings.Create;
  FFmt.DecimalSeparator := '.';
  FDoc := TXMLDocument.Create(nil);
  FDoc.LoadFromXML(xmlString);

  FCurrentNode := FDoc.ChildNodes.FindNode(topNode);
end;

destructor TXMLArchiverIn.Destroy;
begin
  inherited;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : Boolean);
var
  tempNode : IXMLNode;
  s : string;
begin
  tempNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(tempNode) then begin
    s := Trim(tempNode.Text);
    if UpperCase(s) = 'TRUE' then value := True;
    if UpperCase(s) = 'FALSE' then value := False;
  end;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : Byte);
var
  tempNode : IXMLNode;
  s : string;
begin
  tempNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(tempNode) then begin
    s := tempNode.Text;
    value := StrToIntDef(Trim(s), value);
  end;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : Int32);
var
  tempNode : IXMLNode;
  s : string;
begin
  tempNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(tempNode) then begin
    s := tempNode.Text;
    value := StrToIntDef(Trim(s), value);
  end;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : Int16);
var
  tempNode : IXMLNode;
  s : string;
begin
  tempNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(tempNode) then begin
    s := tempNode.Text;
    value := StrToIntDef(Trim(s), value);
  end;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : Int64);
var
  tempNode : IXMLNode;
  s : string;
begin
  tempNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(tempNode) then begin
    s := tempNode.Text;
    value := StrToInt64Def(Trim(s), value);
  end;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : Single);
var
  tempNode : IXMLNode;
  s : string;
begin
  tempNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(tempNode) then begin
    s := tempNode.Text;
    value := StrToFloatDef(Trim(s), value, FFmt);
  end;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : Double);
var
  tempNode : IXMLNode;
  s : string;
begin
  tempNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(tempNode) then begin
    s := tempNode.Text;
    value := StrToFloatDef(Trim(s), value, FFmt);
  end;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : AnsiString);
var
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(tempNode) then begin
    value := AnsiString(tempNode.Text);
  end;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TSerializable);
begin
  //TODO
end;

procedure TXMLArchiverIn.inout(const name : String; buffer : PByte; bufferSize : Integer);
begin
  //TODO
end;

function TXMLArchiverIn.inout2(const name : String; var value : TSerializable) : TSerializable;
var
  tempNode : IXMLNode;
  types : string;
begin
  if Assigned(value) then begin
    FreeAndNil(value);
  end;
  Result := nil;

  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    if FCurrentNode.HasAttribute('type') then begin
      types := FCurrentNode.Attributes['type'];
      Result := FFactory.Make(types);
      if Assigned(Result) then begin
        // We need to preserve the type information since the factory only can create
        // objects it knows how to create, and this can be a more generalized (base) object
        // than the actual one. The rest of the bytes will be placed in the spareBytes member.
        SetTypesString(Result, AnsiString(types));

        Result.serialize(Self);
      end;
    end;
  end;

  FCurrentNode := tempNode;
end;

function TXMLArchiverIn.inout(const name : String; var value : TSerializable; element : Integer) : TSerializable;
var
  i : Integer;
  tempNode : IXMLNode;
  types : string;
begin
  Result := Value;

  // Find n:th element
  for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
    if FCurrentNode.ChildNodes[i].NodeName = 'element' then begin
      if element > 0 then begin
        Dec(element);

      end else begin
        tempNode := FCurrentNode;

        FCurrentNode := FCurrentNode.ChildNodes[i];

        if Assigned(FCurrentNode) then begin
          if FCurrentNode.HasAttribute('type') then begin
            types := FCurrentNode.Attributes['type'];
            Result := FFactory.Make(types);
            if Assigned(Result) then begin
              Result.serialize(Self);
            end;
          end;
        end;

        FCurrentNode := tempNode;
        Break;
      end;
    end;
  end;
end;

function TXMLArchiverIn.numElements : Integer;
var
  i : Integer;
begin
  Result := 0;
  for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
    if FCurrentNode.ChildNodes[i].NodeName = 'element' then Inc(Result);
  end;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynBooleanArray);
var
  i, elem : Integer;
  s : string;
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    // Set new length (elements will be False)
    SetLength(value, numElements);

    elem := 0;
    for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
      if FCurrentNode.ChildNodes[i].NodeName = 'element' then begin
        s := Trim(FCurrentNode.ChildNodes[i].Text);
        if UpperCase(s) = 'TRUE' then value[elem] := True;
        if UpperCase(s) = 'FALSE' then value[elem] := False;
        Inc(elem);
      end;
    end;
  end;

  FCurrentNode := tempNode;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynByteArray);
var
  i, elem : Integer;
  s : string;
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    // Set new length (elements will be 0)
    SetLength(value, numElements);

    elem := 0;
    for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
      if FCurrentNode.ChildNodes[i].NodeName = 'element' then begin
        s := FCurrentNode.ChildNodes[i].Text;
        value[elem] := StrToIntDef(Trim(s), value[elem]);
        Inc(elem);
      end;
    end;
  end;

  FCurrentNode := tempNode;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynInt32Array);
var
  i, elem : Integer;
  s : string;
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    // Set new length (elements will be 0)
    SetLength(value, numElements);

    elem := 0;
    for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
      if FCurrentNode.ChildNodes[i].NodeName = 'element' then begin
        s := FCurrentNode.ChildNodes[i].Text;
        value[elem] := StrToIntDef(Trim(s), value[elem]);
        Inc(elem);
      end;
    end;
  end;

  FCurrentNode := tempNode;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynInt16Array);
var
  i, elem : Integer;
  s : string;
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    // Set new length (elements will be 0)
    SetLength(value, numElements);

    elem := 0;
    for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
      if FCurrentNode.ChildNodes[i].NodeName = 'element' then begin
        s := FCurrentNode.ChildNodes[i].Text;
        value[elem] := StrToIntDef(Trim(s), value[elem]);
        Inc(elem);
      end;
    end;
  end;

  FCurrentNode := tempNode;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynInt64Array);
var
  i, elem : Integer;
  s : string;
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    // Set new length (elements will be 0)
    SetLength(value, numElements);

    elem := 0;
    for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
      if FCurrentNode.ChildNodes[i].NodeName = 'element' then begin
        s := FCurrentNode.ChildNodes[i].Text;
        value[elem] := StrToInt64Def(Trim(s), value[elem]);
        Inc(elem);
      end;
    end;
  end;

  FCurrentNode := tempNode;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynSingleArray);
var
  i, elem : Integer;
  s : string;
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    // Set new length (elements will be 0)
    SetLength(value, numElements);

    elem := 0;
    for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
      if FCurrentNode.ChildNodes[i].NodeName = 'element' then begin
        s := FCurrentNode.ChildNodes[i].Text;
        value[elem] := StrToFloatDef(Trim(s), value[elem], FFmt);
        Inc(elem);
      end;
    end;
  end;

  FCurrentNode := tempNode;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynDoubleArray);
var
  i, elem : Integer;
  s : string;
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    // Set new length (elements will be 0)
    SetLength(value, numElements);

    elem := 0;
    for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
      if FCurrentNode.ChildNodes[i].NodeName = 'element' then begin
        s := FCurrentNode.ChildNodes[i].Text;
        value[elem] := StrToFloatDef(Trim(s), value[elem], FFmt);
        Inc(elem);
      end;
    end;
  end;

  FCurrentNode := tempNode;
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynAnsiStringArray);
var
  i, elem : Integer;
  s : string;
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    // Set new length (elements will be 0)
    SetLength(value, numElements);

    elem := 0;
    for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
      if FCurrentNode.ChildNodes[i].NodeName = 'element' then begin
        s := FCurrentNode.ChildNodes[i].Text;
        value[elem] := AnsiString(s);
        Inc(elem);
      end;
    end;
  end;

  FCurrentNode := tempNode;
end;

procedure TXMLArchiverIn.inout(const name : string; var value : TDynSerializableArray);
var
  i, size : Integer;
  tempNode : IXMLNode;
begin
  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
  if Assigned(FCurrentNode) then begin
    // First free ev existing objects in the array
    for i := 0 to Length(value)-1 do begin
      FreeAndNil(value[i]);
    end;

    // Set new length (elements will be 0)
    size := numElements;
    SetLength(value, size);

    // Now loop over all objects in the array
    for i := 0 to size-1 do begin
      value[i] := inout(name, value[i], i);
    end;
  end;

  FCurrentNode := tempNode;
end;

function TXMLArchiverIn.beginList(const name : String; size : Integer) : Integer;
begin
  //Nothing to do in this implementation
  Result := 0;
end;

procedure TXMLArchiverIn.endList(const name : String);
begin
  //Nothing to do in this implementation
end;

end.

