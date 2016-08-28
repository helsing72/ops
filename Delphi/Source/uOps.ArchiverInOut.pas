unit uOps.ArchiverInOut;

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

uses uOps.Types;

type
  TArchiverInOut = class;

	TSerializable = class(TObject)
  public
		procedure Serialize(archiver : TArchiverInOut); virtual; abstract;
	end;

  TDynSerializableArray = array of TSerializable;

  TArchiverInOut = class(TObject)
  public
    procedure inout(const name : String; var value : Boolean); overload; virtual; abstract;
    procedure inout(const name : String; var value : Byte); overload; virtual; abstract;
    procedure inout(const name : String; var value : Int32); overload; virtual; abstract;
    procedure inout(const name : String; var value : Int16); overload; virtual; abstract;
    procedure inout(const name : String; var value : Int64); overload; virtual; abstract;
    procedure inout(const name : String; var value : Single); overload; virtual; abstract;
    procedure inout(const name : String; var value : Double); overload; virtual; abstract;
    procedure inout(const name : String; var value : AnsiString); overload; virtual; abstract;
    procedure inout(const name : String; var value : TSerializable); overload; virtual; abstract;

		procedure inout(const name : String; buffer : PByte; bufferSize : Integer); overload; virtual; abstract;

		function inout2(const name : String; var value : TSerializable) : TSerializable; overload; virtual; abstract;

    function inout(const name : String; var value : TSerializable; element : Integer) : TSerializable; overload; virtual; abstract;

    procedure inout(const name : String; var value : TDynBooleanArray); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynByteArray); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynInt32Array); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynInt16Array); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynInt64Array); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynSingleArray); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynDoubleArray); overload; virtual; abstract;
    procedure inout(const name : String; var value : TDynAnsiStringArray); overload; virtual; abstract;

    procedure inout(const name : string; var Value : TDynSerializableArray); overload; virtual; abstract;

  protected
    function beginList(const name : String; size : Integer) : Integer; virtual; abstract;
    procedure endList(const name : String); virtual; abstract;
  end;

implementation

end.

