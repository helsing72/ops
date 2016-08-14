unit uLogger;

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

uses Classes, Syncobjs;

type
  // Order is important and used by Add()
  TLogType = (ltInformation, ltWarning, ltError, ltNone);

  // A simple threadsafe logger
  TLogger = class (TObject)
  private
    fList    : TStringList;
    fLogType : TLogType;
    fLock    : TCriticalSection;
    fAddTime : Boolean;
    procedure Add (LogType : TLogType; Str : string);
  public
    constructor Create;
    destructor Destroy; override;

    // Get all stored strings and empty buffer. Normally used by the MMI thread
    procedure Get (Strings : TStrings);

    // Log strings of different severity
    procedure AddInformation( Str : string);
    procedure AddWarning( Str : string);
    procedure AddError( Str : string);

    // Set to have the current time logged together with the string
    property AddTime : Boolean read fAddTime write fAddTime;

    // Defines what to store
    // ltInformation => All types are stored
    // ltWarning     => Warnings and Errors are stored
    // ltError       => Only Errors are stored
    // ltNone        => Nothing are stored
    property LogType : TLogType read fLogType write fLogType;
  end;

(**************************************************************************
* I M P L E M E N T A T I O N
**************************************************************************)
implementation

uses SysUtils;

{ TLogger }

(**************************************************************************
*
**************************************************************************)
constructor TLogger.Create;
begin
  inherited Create;
  fList    := TStringList.Create;
  fLock    := TCriticalSection.Create;
  fLogType := ltInformation;
end;

(**************************************************************************
*
**************************************************************************)
destructor TLogger.Destroy;
begin
  fLock.Free;
  fList.Free;
  inherited Destroy;
end;

(**************************************************************************
*
**************************************************************************)
procedure TLogger.Add (LogType : TLogType; Str : string);
var
  TimeStr : string;
begin
  if LogType >= fLogType then begin
    if fAddTime then begin
      TimeStr := '[' + FormatDateTime('hh:nn:ss.zzz', Now) + '] ';
    end else begin
      TimeStr := '';
    end;
    fLock.Acquire;
    try
      fList.Add (TimeStr + Str);
    finally
      fLock.Release;
    end;
  end;
end;

(**************************************************************************
*
**************************************************************************)
procedure TLogger.Get (Strings : TStrings);
begin
  fLock.Acquire;
  try
    if fList.Count > 0 then begin
      Strings.AddStrings (fList);
      fList.Clear;
    end;
  finally
    fLock.Release;
  end;
end;

(**************************************************************************
*
**************************************************************************)
procedure TLogger.AddError(Str: string);
begin
  Add(ltError, Str);
end;

(**************************************************************************
*
**************************************************************************)
procedure TLogger.AddInformation(Str: string);
begin
  Add(ltInformation, Str);
end;

(**************************************************************************
*
**************************************************************************)
procedure TLogger.AddWarning(Str: string);
begin
  Add(ltWarning, Str);
end;

end.

