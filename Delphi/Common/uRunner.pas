unit uRunner;

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

uses Classes;

type
  TRunProc = procedure of object;

  // Thread that
  TRunner = class(TThread)
  private
    { Private declarations }
    FProc : TRunProc;
  protected
    procedure Execute; override;
  public
    constructor Create(Proc : TRunProc);
    destructor Destroy; override;
    procedure Terminate;

    property Terminated;
  end;

implementation

uses SysUtils;

constructor TRunner.Create(Proc : TRunProc);
begin
  inherited Create;
  FProc := Proc;
end;

destructor TRunner.Destroy;
begin
  Terminate;
  WaitFor;
  inherited;
end;

procedure TRunner.Terminate;
begin
  inherited Terminate;
end;

procedure TRunner.Execute;
begin
  if not Assigned(FProc) then Exit;

  while not Terminated do begin
    try
      FProc;
    except
    end;
  end;
end;

end.

