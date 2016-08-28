unit uOps.RequestReply;

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

uses uOps.OPSObject,
     uOps.Topic,
     uOps.Subscriber,
     uOps.Publisher,
     uOps.KeyFilterQoSPolicy,
     uOps.RequestReply.Request,
     uOps.RequestReply.Reply;

type
  TRequestReply<ReqType : TRequest; RepType : TReply> = class(TObject)
  private
		FSub : TSubscriber;
		FPub : TPublisher;
		FKey : string;
    FReqInt : Integer;

  public
    constructor Create(reqTopic : TTopic; repTopic : TTopic; key : string);
    destructor Destroy; override;

    // Sends the req message and waits for a reply message with timeout.
    // Returns the reply message or nil if no reply within the given timeout.
    function Request(req : ReqType; timeoutMS : Integer) : RepType;
  end;

implementation

uses SysUtils,
     uOps.TimeHelper;

constructor TRequestReply<ReqType, RepType>.Create(reqTopic : TTopic; repTopic : TTopic; key : string);
begin
  inherited Create;
  FKey := key;

  FPub := TPublisher.Create(reqTopic);
  FPub.Key := AnsiString(FKey);

  FSub := TSubscriber.Create(repTopic);
  FSub.addFilterQoSPolicy(TKeyFilterQoSPolicy.Create(FKey));
  FSub.Start;
end;

destructor TRequestReply<ReqType, RepType>.Destroy;
begin
  FreeAndNil(FSub);
  FreeAndNil(FPub);
  inherited;
end;

function TRequestReply<ReqType, RepType>.Request(req : ReqType; timeoutMS : Integer) : RepType;
var
  requestLimit : Int64;
begin
  Result := nil;
  Inc(FReqInt);
  req.requestId := AnsiString(IntToStr(FReqInt));

  FSub.getDataReference();  // Clear new data flag in subscriber

  FPub.writeOPSObject(req); // Send request

  requestLimit := TTimeHelper.CurrentTimeMillis + Int64(timeoutMS);

  while (TTimeHelper.currentTimeMillis < requestLimit) do begin
    if FSub.waitForNewData(Integer(requestLimit - TTimeHelper.currentTimeMillis)) then begin
      FSub.aquireMessageLock;
      try
        // Call to getMessage clears new data flag in subscriber
        if RepType(FSub.getMessage.Data).requestId = req.requestId then begin
          Result := RepType(FSub.getMessage.Data.Clone);
        end;
      finally
        FSub.releaseMessageLock;
      end;
    end;
  end;
end;

end.

