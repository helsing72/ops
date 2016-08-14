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
     Xml.XmlDoc,
     Xml.XmlDom,
     Xml.XmlIntf,
     uOps.Types,
     uOPs.BasicError,
     uOps.ArchiverInOut,
     uOps.SerializableInheritingTypeFactory;

type
  TXMLArchiverIn = class(TArchiverInOut)
  private
    FFactory : TSerializableInheritingTypeFactory;
    FDoc : IXMLDocument;
    FCurrentNode: IXMLNode;
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

uses SysUtils;

constructor TXMLArchiverIn.Create(xmlString : string; topNode : string; factory : TSerializableInheritingTypeFactory);
begin
  FFactory := factory;

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
    s := tempNode.Text;
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
    value := StrToInt(s);
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
    value := StrToInt(s);
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
    value := StrToInt(s);
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
    value := StrToInt64(s);
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
    value := StrToFloat(s)
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
    value := StrToFloat(s)
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

procedure TXMLArchiverIn.inout(const name : String; buffer : PByte; bufferSize : Integer);
begin
  //TODO
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TSerializable);
begin
  //TODO
end;

function TXMLArchiverIn.inout2(const name : String; var value : TSerializable) : TSerializable;
var
  tempNode : IXMLNode;
  types : string;
begin
  Result := Value;

  tempNode := FCurrentNode;

  FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
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

procedure TXMLArchiverIn.inout(const name : String; var value : TDynBooleanArray);
begin
//            if (!currentNode.getChildNode(name.c_str()).isEmpty())
//            {
//                opsXML::XMLNode tempNode = currentNode;
//                currentNode = currentNode.getChildNode(name.c_str());
//
//                int size = currentNode.nChildNode("element");
//                value.reserve(size);
//                value.resize(size, false);
//                for (int i = 0; i < size; i++)
//                {
//                    string s(currentNode.getChildNode("element").getText());
//                    if (s.compare("true") == 0) value[i] = true;
//                    if (s.compare("false") == 0) value[i] = false;
//                    if (s.compare("TRUE") == 0) value[i] = true;
//                    if (s.compare("FALSE") == 0) value[i] = false;
//                    if (s.compare("true") == 0) value[i] = true;
//                    if (s.compare("false") == 0) value[i] = false;
//                    if (s.compare("True") == 0) value[i] = true;
//                    if (s.compare("False") == 0) value[i] = false;
//                }
//
//                currentNode = tempNode;
//            }
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynByteArray);
begin
//            if (!currentNode.getChildNode(name.c_str()).isEmpty())
//            {
//                opsXML::XMLNode tempNode = currentNode;
//                currentNode = currentNode.getChildNode(name.c_str());
//
//                int size = currentNode.nChildNode("element");
//                value.reserve(size);
//                value.resize(size, 0);
//                for (int i = 0; i < size; i++)
//                {
//                    string s(currentNode.getChildNode("element").getText());
//                    stringstream ss(s);
//
//                    int inVal;
//                    ss >> inVal;
//                    value[i] = inVal;
//                }
//
//                currentNode = tempNode;
//            }
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynInt32Array);
begin
//            if (!currentNode.getChildNode(name.c_str()).isEmpty())
//            {
//                opsXML::XMLNode tempNode = currentNode;
//                currentNode = currentNode.getChildNode(name.c_str());
//
//                int size = currentNode.nChildNode("element");
//                value.reserve(size);
//                value.resize(size, 0);
//                for (int i = 0; i < size; i++)
//                {
//                    string s(currentNode.getChildNode("element").getText());
//                    stringstream ss(s);
//
//                    int inVal;
//                    ss >> inVal;
//                    value[i] = inVal;
//                }
//
//                currentNode = tempNode;
//            }
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynInt16Array);
begin
//            if (!currentNode.getChildNode(name.c_str()).isEmpty())
//            {
//                opsXML::XMLNode tempNode = currentNode;
//                currentNode = currentNode.getChildNode(name.c_str());
//
//                int size = currentNode.nChildNode("element");
//                value.reserve(size);
//                value.resize(size, 0);
//                for (int i = 0; i < size; i++)
//                {
//                    string s(currentNode.getChildNode("element").getText());
//                    stringstream ss(s);
//
//                    int inVal;
//                    ss >> inVal;
//                    value[i] = inVal;
//                }
//
//                currentNode = tempNode;
//            }
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynInt64Array);
begin
//            if (!currentNode.getChildNode(name.c_str()).isEmpty())
//            {
//                opsXML::XMLNode tempNode = currentNode;
//                currentNode = currentNode.getChildNode(name.c_str());
//
//                int size = currentNode.nChildNode("element");
//                value.reserve(size);
//                value.resize(size, 0);
//                for (int i = 0; i < size; i++)
//                {
//                    string s(currentNode.getChildNode("element").getText());
//                    stringstream ss(s);
//
//                    int inVal;
//                    ss >> inVal;
//                    value[i] = inVal;
//                }
//
//                currentNode = tempNode;
//            }
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynSingleArray);
begin
//            if (!currentNode.getChildNode(name.c_str()).isEmpty())
//            {
//                opsXML::XMLNode tempNode = currentNode;
//                currentNode = currentNode.getChildNode(name.c_str());
//
//                int size = currentNode.nChildNode("element");
//                value.reserve(size);
//                value.resize(size, 0.0);
//                for (int i = 0; i < size; i++)
//                {
//                    string s(currentNode.getChildNode("element").getText());
//                    stringstream ss(s);
//
//                    float inVal;
//                    ss >> inVal;
//                    value[i] = inVal;
//                }
//
//                currentNode = tempNode;
//            }
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynDoubleArray);
begin
//            if (!currentNode.getChildNode(name.c_str()).isEmpty())
//            {
//                opsXML::XMLNode tempNode = currentNode;
//                currentNode = currentNode.getChildNode(name.c_str());
//
//                int size = currentNode.nChildNode("element");
//                value.reserve(size);
//                value.resize(size, 0.0);
//                for (int i = 0; i < size; i++)
//                {
//                    string s(currentNode.getChildNode("element").getText());
//                    stringstream ss(s);
//
//                    double inVal;
//                    ss >> inVal;
//                    value[i] = inVal;
//                }
//
//                currentNode = tempNode;
//            }
end;

procedure TXMLArchiverIn.inout(const name : String; var value : TDynAnsiStringArray);
begin
//            if (!currentNode.getChildNode(name.c_str()).isEmpty())
//            {
//                opsXML::XMLNode tempNode = currentNode;
//                currentNode = currentNode.getChildNode(name.c_str());
//
//                int size = currentNode.nChildNode("element");
//                value.reserve(size);
//                value.resize(size, "");
//                for (int i = 0; i < size; i++)
//                {
//                    string s(currentNode.getChildNode("element").getText());
//                    stringstream ss(s);
//
//                    int inVal;
//                    ss >> inVal;
//                    value[i] = inVal;
//                }
//
//                currentNode = tempNode;
//            }
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

    size := 0;
    for i := 0 to FCurrentNode.ChildNodes.Count - 1 do begin
      if FCurrentNode.ChildNodes[i].NodeName = 'element' then Inc(size);
    end;

    // Set new length (elements will be nil)
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

