unit uOps.ParticipantInfoDataListener;

(**
*
* Copyright (C) 2016-2017 Lennart Andersson.
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

uses
  System.SyncObjs,
  uNotifier,
  uOps.Error,
  uOps.Topic,
  uOps.Domain,
  uOps.OPSMessage,
  uOPs.ParticipantInfoData,
  uOps.Transport.SendDataHandler;

type
  // Method prototype to call when we want to check if a specific topic is published on the participant
  TOnHasPublisherOnProc = function(topicName : AnsiString) : Boolean of object;

	TParticipantInfoDataListener = class(TObject)
  private
    // Borrowed references
    FDomain : TDomain;
    FPartInfoTopic : TTopic;
    FErrorService : TErrorService;
    FSendDataHandler : TSendDataHandler;
    FOnHasPublisherOnProc : TOnHasPublisherOnProc;

    //
    FMutex : TMutex;
    FPartInfoSub : TObject;   // Anonymous to avoid circular dependencies

    FNumUdpTopics : Integer;

    function setupSubscriber : Boolean;
    procedure removeSubscriber;

    procedure OnOpsMessage(Sender : TObject; obj : TOPSMessage);

    function HasPublisherOn(name : AnsiString) : Boolean;

  public
    constructor Create(dom : TDomain; partInfoTopic : TTopic; Proc : TOnHasPublisherOnProc; Reporter : TErrorService);
    destructor Destroy; override;

//		void prepareForDelete();

    procedure connectUdp(top : TTopic; handler : TSendDataHandler);
    procedure disconnectUdp(top : TTopic; handler : TSendDataHandler);

    procedure connectTcp(top : TTopic; handler : TObject);
    procedure disconnectTcp(top : TTopic; handler : TObject);
  end;

implementation

uses SysUtils,
     uOps.NetworkSupport,
     uOps.Subscriber,
     uOPs.Transport.McUdpSendDataHandler;

{ TParticipantInfoDataListener }

constructor TParticipantInfoDataListener.Create(dom : TDomain; partInfoTopic : TTopic;
                Proc : TOnHasPublisherOnProc; Reporter : TErrorService);
begin
  inherited Create;
  FDomain := dom;
  FPartInfoTopic := partInfoTopic;
  FOnHasPublisherOnProc := Proc;
  FErrorService := Reporter;
  FMutex := TMutex.Create;
end;

destructor TParticipantInfoDataListener.Destroy;
begin
  removeSubscriber;       // Just to be sure
  FreeAndNil(FMutex);
  inherited;
end;

function TParticipantInfoDataListener.HasPublisherOn(name : AnsiString) : Boolean;
begin
  if Assigned(FOnHasPublisherOnProc) then begin
    Result := FOnHasPublisherOnProc(name);
  end else begin
    Result := False;
  end;
end;

procedure TParticipantInfoDataListener.connectUdp(top: TTopic; handler: TSendDataHandler);
begin
  FMutex.Acquire;
  try
    if not Assigned(FPartInfoSub) then begin
      if not setupSubscriber then begin
        if not isValidNodeAddress(top.DomainAddress) then begin
          // Generate an error message if we come here with domain->getMetaDataMcPort() == 0,
          // it means that we have UDP topics that require meta data but user has disabled it.
          if Assigned(FErrorService) then begin
            FErrorService.Report(TBasicError.Create(
              'ParticipantInfoDataListener', 'connectUdp',
              'UDP topic "' + string(top.Name) + '" won''t work since Meta Data disabled in config-file'));
          end;
        end;
      end;
    end;

    // Since we only have one common UDP SendDataHandler, its enough to count connected topics
    Inc(FNumUdpTopics);

    FSendDataHandler := handler;
  finally
    FMutex.Release;
  end;
end;

procedure TParticipantInfoDataListener.disconnectUdp(top: TTopic; handler: TSendDataHandler);
begin
  FMutex.Acquire;
  try
    // Remove topic from list so we know if the subscriber is needed
    Dec(FNumUdpTopics);

    if FNumUdpTopics = 0 then begin
      FSendDataHandler := nil;

//TODO  if num tcp topics = 0 then begin
        removeSubscriber();
//    end;
    end;
  finally
    FMutex.Release;
  end;
end;

procedure TParticipantInfoDataListener.connectTcp(top: TTopic;
  handler: TObject);
begin
  FMutex.Acquire;
  try
    if not Assigned(FPartInfoSub) then begin
      if not setupSubscriber then begin
        // Generate an error message if we come here with domain->getMetaDataMcPort() == 0,
        // it means that we have TCP topics that require meta data but user has disabled it.
        if Assigned(FErrorService) then begin
          FErrorService.Report(TBasicError.Create(
            'ParticipantInfoDataListener', 'connectTcp',
            'TCP topic "' + string(top.Name) + '" won''t work since Meta Data disabled in config-file'));
        end;
        Exit;
      end;
    end;

    ///TODO add to map

  finally
    FMutex.Release;
  end;
end;

procedure TParticipantInfoDataListener.disconnectTcp(top: TTopic;
  handler: TObject);
begin
  FMutex.Acquire;
  try
    ///TODO remove from map
  finally
    FMutex.Release;
  end;
end;

function TParticipantInfoDataListener.setupSubscriber: Boolean;
begin
  Result := False;

  // Check that user hasn't disabled the meta data
  if FDomain.MetaDataMcPort = 0 then Exit;

  FPartInfoSub := TSubscriber.Create(FPartInfoTopic);
  TSubScriber(FPartInfoSub).AddDataListener(OnOpsMessage);
  TSubScriber(FPartInfoSub).Start;

  Result := True;
end;

procedure TParticipantInfoDataListener.removeSubscriber;
begin
  FreeAndNil(FPartInfoSub);
end;

procedure TParticipantInfoDataListener.OnOpsMessage(Sender : TObject; obj : TOPSMessage);
var
  i : Integer;
  partInfo : TParticipantInfoData;
begin
  if Assigned(obj) then begin
    if obj.Data is TParticipantInfoData then begin
      partInfo := obj.Data as TParticipantInfoData;
      if Assigned(partInfo) then begin
        // Is it on our domain?
        if partInfo.Domain = FDomain.DomainID then begin
          FMutex.Acquire;
          try
            // Checks for UDP transport
            if Assigned(FSendDataHandler) and Assigned(partInfo.subscribeTopics) then begin
              if partInfo.mc_udp_port <> 0 then begin
                for i := 0 to Length(partInfo.subscribeTopics) - 1 do begin
                  if (partInfo.subscribeTopics[i].transport = TTopic.TRANSPORT_UDP) and
                     HasPublisherOn(partInfo.subscribeTopics[i].name)
                  then begin
                    // Do an add sink here
                    TMcUdpSendDataHandler(FSendDataHandler).addSink(
                        string(partInfo.subscribeTopics[i].name), string(partInfo.ip), partInfo.mc_udp_port);
                  end;
                end;
              end;
            end;

            // Checks for TCP transport
//          for (unsigned int i = 0; i < partInfo->publishTopics.size(); i++)
//          {
//						if (partInfo->publishTopics[i].transport == Topic::TRANSPORT_TCP) {
//							///TODO lookup topic in map. If found call handler
//            }
//          }
          finally
            FMutex.Release;
          end;
        end;
      end;
    end else begin
      if Assigned(FErrorService) then begin
        FErrorService.Report(TBasicError.Create(
          'ParticipantInfoDataListener', 'onNewData', 'Data could not be cast as expected.'));
      end;
    end;
  end;
end;

end.

