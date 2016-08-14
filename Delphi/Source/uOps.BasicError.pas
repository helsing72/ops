unit uOps.BasicError;

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

type
  // Interface for errors in OPS
	TError = class(TObject)
  public
    function getErrorCode : Integer; virtual; abstract;
    function getMessage : string; virtual; abstract;
  end;

  // Basic implementaion of an error for OPS.
  TBasicError = class(TError)
  public
    const
      ERROR_CODE = 1;
  protected
    FMessage : string;
    FClassName : string;
    FMethod : string;
    FErrorCode : Integer;
  public
    constructor Create(className, method, mess : string);
    function getErrorCode : Integer; override;
    function getMessage : string; override;
  end;

implementation

constructor TBasicError.Create(className, method, mess : string);
begin
  inherited Create;
  FMessage := mess;
  FClassName := ClassName;
  FMethod := method;
  FErrorCode := ERROR_CODE;
end;

function TBasicError.getErrorCode : Integer;
begin
  Result := FErrorCode;
end;

function TBasicError.getMessage : string;
begin
  Result := FClassName + '.' + FMethod + '(): ' + FMessage;
end;

end.

