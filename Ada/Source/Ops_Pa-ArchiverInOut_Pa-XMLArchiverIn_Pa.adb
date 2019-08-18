--
-- Copyright (C) 2017-2019 Lennart Andersson.
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

with Ada.Text_IO;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Input_Sources.Strings;
with Unicode.CES;
with Unicode.CES.Utf8;
with Sax.Readers;
with DOM.Core.Nodes;

with Ops_Pa.SerializableFactory_Pa.CompFactory_Pa;
use  Ops_Pa.SerializableFactory_Pa.CompFactory_Pa;

package body Ops_Pa.ArchiverInOut_Pa.XMLArchiverIn_Pa is

  use type DOM.Core.Node;

  package Int64_IO is new Ada.Text_IO.Integer_IO(Int64); use Int64_IO;
  package Int32_IO is new Ada.Text_IO.Integer_IO(Int32); use Int32_IO;
  package Int16_IO is new Ada.Text_IO.Integer_IO(Int16); use Int16_IO;

  package Byte_IO is new Ada.Text_IO.Modular_IO(Byte); use Byte_IO;


  function Create( xmlString : string;
                   topNode : string;
                   fact : SerializableInheritingTypeFactory_Class_At ) return XMLArchiverIn_Class_At is
    Self : XMLArchiverIn_Class_At := null;
  begin
    Self := new XMLArchiverIn_Class;
    InitInstance( Self.all, Self, xmlString, topNode, fact );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  function FindNode( nlist : DOM.Core.Node_List; name : String ) return DOM.Core.Node is
    node : DOM.Core.Node := null;
  begin
    for idx in 0 .. DOM.Core.Nodes.Length( nlist )-1 loop
      node := DOM.Core.Nodes.Item( nlist, idx );
      if DOM.Core.Nodes.Node_Name( node ) = name then
        return node;
      end if;
    end loop;
    return null;
  end;

  function numElements( nlist : DOM.Core.Node_List ) return Integer is
    node : DOM.Core.Node := null;
    Result : Integer := 0;
  begin
    for idx in 0 .. DOM.Core.Nodes.Length( nlist )-1 loop
      node := DOM.Core.Nodes.Item( nlist, idx );
      if DOM.Core.Nodes.Node_Name( node ) = "element" then
        Result := Result + 1;
      end if;
    end loop;
    return Result;
  end;

  procedure InitInstance( Self : in out XMLArchiverIn_Class;
                          SelfAt : XMLArchiverIn_Class_At;
                          xmlString : String;
                          topNode : String;
                          fact : SerializableInheritingTypeFactory_Class_At) is
    Input : Input_Sources.Strings.String_Input;
  begin
    Self.SelfAt := SelfAt;
    Self.Factory := fact;

    Input_Sources.Strings.
      Open(Str => Unicode.CES.Byte_Sequence(xmlString),
           Encoding => Unicode.CES.Utf8.Utf8_Encoding,
           Input => Input);

    DOM.Readers.Set_Feature( Self.Reader, Sax.Readers.Validation_Feature, False );
    DOM.Readers.Set_Feature( Self.Reader, Sax.Readers.Namespace_Feature, False );

    DOM.Readers.Parse( Self.Reader, Input );

    Input_Sources.Strings.Close( Input );

    Self.Doc := DOM.Readers.Get_Tree( Self.Reader );

--    DOM.Core.Nodes.Dump(Self.Doc);

--    Ada.Text_IO.Put_Line("--->>> Node_Name: " & DOM.Core.Nodes.Node_Name( Self.Doc ));

--      declare
--        list : DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes( Self.Doc );
--      begin
--        Ada.Text_IO.Put_Line("--->>> Length(): " & Natural'Image(DOM.Core.Nodes.Length(list)));
--        for idx in 0..DOM.Core.Nodes.Length(list)-1 loop
--          Ada.Text_IO.Put_Line("--->>> Node_Name: " & DOM.Core.Nodes.Node_Name( DOM.Core.Nodes.Item(list, idx)));
--        end loop;
--      end;

    Self.CurrentNode := FindNode( DOM.Core.Nodes.Child_Nodes( Self.Doc ), topNode );

--    Ada.Text_IO.Put_Line("--->>> Node_Name: " & DOM.Core.Nodes.Node_Name( Self.CurrentNode ));
  end;

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out XMLArchiverIn_Class ) is
  begin
    DOM.Readers.Free( Self.Reader );
  end;

  function IsOut( Self : in XMLArchiverIn_Class) return Boolean is
  begin
    return False;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Boolean) is
--      tempNode : IXMLNode;
--      s : string;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(tempNode) then
--        s := Trim(tempNode.Text);
--        if UpperCase(s) = "TRUE" then value := True; end if;
--        if UpperCase(s) = "FALSE" then value := False; end if;
--      end if;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Byte) is
    tempNode : DOM.Core.Node := FindNode( DOM.Core.Nodes.Child_Nodes( Self.CurrentNode ), name );
  begin
    if tempNode /= null and then DOM.Core.Nodes.Has_Child_Nodes( tempNode ) then
      tempNode := DOM.Core.Nodes.First_Child( tempNode );
      declare
        s : String := Trim(DOM.Core.Nodes.Node_Value( tempNode ), Ada.Strings.Both);
        last : Positive;
      begin
        Get(s, value, last);
      exception
        when others =>
          null;
      end;
    end if;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int32) is
    tempNode : DOM.Core.Node := FindNode( DOM.Core.Nodes.Child_Nodes( Self.CurrentNode ), name );
  begin
    if tempNode /= null and then DOM.Core.Nodes.Has_Child_Nodes( tempNode ) then
      tempNode := DOM.Core.Nodes.First_Child( tempNode );
      declare
        s : String := Trim(DOM.Core.Nodes.Node_Value( tempNode ), Ada.Strings.Both);
        last : Positive;
      begin
        Get(s, value, last);
      exception
        when others =>
          null;
      end;
    end if;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int16) is
    tempNode : DOM.Core.Node := FindNode( DOM.Core.Nodes.Child_Nodes( Self.CurrentNode ), name );
  begin
    if tempNode /= null and then DOM.Core.Nodes.Has_Child_Nodes( tempNode ) then
      tempNode := DOM.Core.Nodes.First_Child( tempNode );
      declare
        s : String := Trim(DOM.Core.Nodes.Node_Value( tempNode ), Ada.Strings.Both);
        last : Positive;
      begin
        Get(s, value, last);
      exception
        when others =>
          null;
      end;
    end if;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int64) is
    tempNode : DOM.Core.Node := FindNode( DOM.Core.Nodes.Child_Nodes( Self.CurrentNode ), name );
  begin
    if tempNode /= null and then DOM.Core.Nodes.Has_Child_Nodes( tempNode ) then
      tempNode := DOM.Core.Nodes.First_Child( tempNode );
      declare
        s : String := Trim(DOM.Core.Nodes.Node_Value( tempNode ), Ada.Strings.Both);
        last : Positive;
      begin
        Get(s, value, last);
      exception
        when others =>
          null;
      end;
    end if;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float32) is
--      tempNode : IXMLNode;
--      s : string;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(tempNode) then
--        s := tempNode.Text;
--        value := StrToFloatDef(Trim(s), value, FFmt);
--      end if;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float64) is
--      tempNode : IXMLNode;
--      s : string;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(tempNode) then
--        s := tempNode.Text;
--        value := StrToFloatDef(Trim(s), value, FFmt);
--      end if;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out String_At) is
    tempNode : DOM.Core.Node := FindNode( DOM.Core.Nodes.Child_Nodes( Self.CurrentNode ), name );
  begin
    if tempNode /= null and then DOM.Core.Nodes.Has_Child_Nodes( tempNode ) then
      tempNode := DOM.Core.Nodes.First_Child( tempNode );
      Replace(value, DOM.Core.Nodes.Node_Value( tempNode ));
    end if;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Serializable_Class_At) is
  begin
    raise Not_Yet_Implemented;
  end;

  overriding function inout2( Self : in out XMLArchiverIn_Class; name : String; value : in out Serializable_Class_At) return Serializable_Class_At is
    tempNode : DOM.Core.Node := Self.CurrentNode;
    Result : Serializable_Class_At := null;
  begin
    if value /= null then
      Free(value);
      value := null;
    end if;

    Self.CurrentNode := FindNode( DOM.Core.Nodes.Child_Nodes( Self.CurrentNode ), name);
    if Self.CurrentNode /= null then
      declare
        Attr : DOM.Core.Named_Node_Map := DOM.Core.Nodes.Attributes( Self.CurrentNode );
        node : DOM.Core.Node := DOM.Core.Nodes.Get_Named_Item( Attr, "type" );
      begin
        if node /= null then
          declare
            types : String := DOM.Core.Nodes.Node_Value( node );
          begin
            Result := Self.Factory.Make(types);
            if Result /= null then
              -- We need to preserve the type information since the factory only can create
              -- objects it knows how to create, and this can be a more generalized (base) object
              -- than the actual one. The rest of the bytes will be placed in the spareBytes member.
              Self.SetTypesString( Result, types);

              Result.serialize( ArchiverInOut_Class_At(Self.SelfAt) );
            end if;
          end;
        end if;
      end;
    end if;
    Self.CurrentNode := tempNode;
    return Result;
  end;

  overriding function inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Serializable_Class_At; element : Integer) return Serializable_Class_At is
    node : DOM.Core.Node := FindNode( DOM.Core.Nodes.Child_Nodes( Self.CurrentNode ), name );
    nlist : DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes( node );
    Result : Serializable_Class_At := null;
    Count : Integer := element;
  begin
    if value /= null then
      Free(value);
      value := null;
    end if;

    -- Find n:th element
    for idx in 0 .. DOM.Core.Nodes.Length( nlist )-1 loop
      node := DOM.Core.Nodes.Item( nlist, idx );
      if DOM.Core.Nodes.Node_Name( node ) = "element" then
        if Count > 0 then
          Count := Count - 1;
        else
          declare
            tempNode : DOM.Core.Node := Self.CurrentNode;
          begin
            Self.CurrentNode := node;

            if Self.CurrentNode /= null then
              declare
                Attr : DOM.Core.Named_Node_Map := DOM.Core.Nodes.Attributes( Self.CurrentNode );
                node : DOM.Core.Node := DOM.Core.Nodes.Get_Named_Item( Attr, "type" );
              begin
                if node /= null then
                  declare
                    types : String := DOM.Core.Nodes.Node_Value( node );
                  begin
                    Result := Self.Factory.Make(types);
                    if Result /= null then
                      Result.serialize( ArchiverInOut_Class_At(Self.SelfAt) );
                    end if;
                  end;
                end if;
              end;
            end if;

            Self.CurrentNode := tempNode;
            exit;
          end;
        end if;
      end if;
    end loop;
    return Result;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Boolean_Arr_At) is
--      i, elem : Integer;
--      s : string;
--      tempNode : IXMLNode;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode;
--
--      FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(FCurrentNode) then
--        -- Set new length (elements will be False)
--        SetLength(value, numElements);
--
--        elem := 0;
--        for i := 0 to FCurrentNode.ChildNodes.Count - 1 loop
--          if FCurrentNode.ChildNodes[i].NodeName = "element" then
--            s := Trim(FCurrentNode.ChildNodes[i].Text);
--            if UpperCase(s) = "TRUE" then value[elem] := True; end if;
--            if UpperCase(s) = "FALSE" then value[elem] := False; end if;
--            Inc(elem);
--          end if;
--        end loop;
--      end if;
--
--      FCurrentNode := tempNode;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Boolean_Arr) is
  begin
    raise Not_Yet_Implemented;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Byte_Arr_At) is
--      i, elem : Integer;
--      s : string;
--      tempNode : IXMLNode;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode;
--
--      FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(FCurrentNode) then
--        -- Set new length (elements will be 0)
--        SetLength(value, numElements);
--
--        elem := 0;
--        for i := 0 to FCurrentNode.ChildNodes.Count - 1 loop
--          if FCurrentNode.ChildNodes[i].NodeName = "element" then
--            s := FCurrentNode.ChildNodes[i].Text;
--            value[elem] := StrToIntDef(Trim(s), value[elem]);
--            Inc(elem);
--          end if;
--        end loop;
--      end if;
--
--      FCurrentNode := tempNode;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Byte_Arr) is
  begin
    raise Not_Yet_Implemented;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int32_Arr_At) is
--      i, elem : Integer;
--      s : string;
--      tempNode : IXMLNode;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode;
--
--      FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(FCurrentNode) then
--        -- Set new length (elements will be 0)
--        SetLength(value, numElements);
--
--        elem := 0;
--        for i := 0 to FCurrentNode.ChildNodes.Count - 1 loop
--          if FCurrentNode.ChildNodes[i].NodeName = "element" then
--            s := FCurrentNode.ChildNodes[i].Text;
--            value[elem] := StrToIntDef(Trim(s), value[elem]);
--            Inc(elem);
--          end if;
--        end loop;
--      end if;
--
--      FCurrentNode := tempNode;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int32_Arr) is
  begin
    raise Not_Yet_Implemented;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int16_Arr_At) is
--      i, elem : Integer;
--      s : string;
--      tempNode : IXMLNode;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode;
--
--      FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(FCurrentNode) then
--        -- Set new length (elements will be 0)
--        SetLength(value, numElements);
--
--        elem := 0;
--        for i := 0 to FCurrentNode.ChildNodes.Count - 1 loop
--          if FCurrentNode.ChildNodes[i].NodeName = "element" then
--            s := FCurrentNode.ChildNodes[i].Text;
--            value[elem] := StrToIntDef(Trim(s), value[elem]);
--            Inc(elem);
--          end if;
--        end loop;
--      end if;
--
--      FCurrentNode := tempNode;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int16_Arr) is
  begin
    raise Not_Yet_Implemented;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int64_Arr_At) is
--      i, elem : Integer;
--      s : string;
--      tempNode : IXMLNode;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode;
--
--      FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(FCurrentNode) then
--        -- Set new length (elements will be 0)
--        SetLength(value, numElements);
--
--        elem := 0;
--        for i := 0 to FCurrentNode.ChildNodes.Count - 1 loop
--          if FCurrentNode.ChildNodes[i].NodeName = "element" then
--            s := FCurrentNode.ChildNodes[i].Text;
--            value[elem] := StrToInt64Def(Trim(s), value[elem]);
--            Inc(elem);
--          end if;
--        end loop;
--      end if;
--
--      FCurrentNode := tempNode;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Int64_Arr) is
  begin
    raise Not_Yet_Implemented;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float32_Arr_At) is
--      i, elem : Integer;
--      s : string;
--      tempNode : IXMLNode;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode;
--
--      FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(FCurrentNode) then
--        -- Set new length (elements will be 0)
--        SetLength(value, numElements);
--
--        elem := 0;
--      for i := 0 to FCurrentNode.ChildNodes.Count - 1 loop
--          if FCurrentNode.ChildNodes[i].NodeName = "element" then
--            s := FCurrentNode.ChildNodes[i].Text;
--            value[elem] := StrToFloatDef(Trim(s), value[elem], FFmt);
--            Inc(elem);
--          end if;
--        end loop;
--      end if;
--
--      FCurrentNode := tempNode;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float32_Arr) is
  begin
    raise Not_Yet_Implemented;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float64_Arr_At) is
--      i, elem : Integer;
--      s : string;
--      tempNode : IXMLNode;
  begin
    raise Not_Yet_Implemented;
--      tempNode := FCurrentNode;
--
--      FCurrentNode := FCurrentNode.ChildNodes.FindNode(name);
--      if Assigned(FCurrentNode) then
--        -- Set new length (elements will be 0)
--        SetLength(value, numElements);
--
--        elem := 0;
--        for i := 0 to FCurrentNode.ChildNodes.Count - 1 loop
--          if FCurrentNode.ChildNodes[i].NodeName = "element" then
--            s := FCurrentNode.ChildNodes[i].Text;
--            value[elem] := StrToFloatDef(Trim(s), value[elem], FFmt);
--            Inc(elem);
--          end if;
--        end loop;
--      end if;
--
--      FCurrentNode := tempNode;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out Float64_Arr) is
  begin
    raise Not_Yet_Implemented;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out String_Arr_At) is
    node : DOM.Core.Node := FindNode( DOM.Core.Nodes.Child_Nodes( Self.CurrentNode ), name );
    nlist : DOM.Core.Node_List := DOM.Core.Nodes.Child_Nodes( node );
    num : Integer := 0;
    elem : Integer := 0;
  begin
    -- Dispose ev strings in array
    Clear(value);
    if value /= null then
      Dispose(value);
      value := null;
    end if;

    num := numElements( nlist );
    if num > 0 then
      -- Set new length (elements will be 0)
      value := new String_Arr(0..num-1);
      value.all := (others => null);

      for idx in 0 .. DOM.Core.Nodes.Length( nlist )-1 loop
        node := DOM.Core.Nodes.Item( nlist, idx );
        if DOM.Core.Nodes.Node_Name( node ) = "element" then
          node := DOM.Core.Nodes.First_Child( node );
          value(elem) := Copy(DOM.Core.Nodes.Node_Value( node ));
          elem := elem + 1;
        end if;
      end loop;
    end if;
  end;

  overriding procedure inout( Self : in out XMLArchiverIn_Class; name : String; value : in out String_Arr) is
  begin
    raise Not_Yet_Implemented;
  end;

  overriding function beginList( Self : in out XMLArchiverIn_Class; name : String; size : Integer) return Integer is
    tempNode : DOM.Core.Node := FindNode( DOM.Core.Nodes.Child_Nodes( Self.CurrentNode ), name );
  begin
    if tempNode /= null then
      return numElements( DOM.Core.Nodes.Child_Nodes( tempNode ));
    else
      return 0;
    end if;
  end;

  overriding procedure endList( Self : in out XMLArchiverIn_Class; name : String) is
  begin
    --Nothing to do in this implementation
    null;
  end;

end Ops_Pa.ArchiverInOut_Pa.XMLArchiverIn_Pa;

