unit uOps.Error;

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

uses uNotifier;

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

  TErrorService = class(TObject)
  private
    FErrorNotifier : TNotifier<TError>;
  public
    constructor Create;
    destructor Destroy; override;

    // Note that several threads can at the same time report errors so
    // listeners need to take that into account
    procedure addListener(Proc : TOnNotifyEvent<TError>);
    procedure removeListener(Proc : TOnNotifyEvent<TError>);

    // The ErrorService takes ower ownership of error objects from the caller
    // They will be free'd when all listeners have been notified
    procedure Report(Error : TError);
  end;

var
  gStaticErrorService : TErrorService;

implementation

uses SysUtils;

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

{ TErrorService }

constructor TErrorService.Create;
begin
  inherited Create;
  FErrorNotifier := TNotifier<TError>.Create(Self);
end;

destructor TErrorService.Destroy;
begin
  FreeAndNil(FErrorNotifier);
  inherited;
end;

procedure TErrorService.addListener(Proc: TOnNotifyEvent<TError>);
begin
  FErrorNotifier.addListener(Proc);
end;

procedure TErrorService.removeListener(Proc: TOnNotifyEvent<TError>);
begin
  FErrorNotifier.removeListener(Proc);
end;

procedure TErrorService.Report(Error: TError);
begin
  try
    FErrorNotifier.doNotify(Error);
  except
  end;
  FreeAndNil(Error);
end;

initialization
  gStaticErrorService := TErrorService.Create;

finalization
  FreeAndNil(gStaticErrorService);

end.

