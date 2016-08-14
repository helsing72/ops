unit uOps.Participant;

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
     uNotifier,
     uOps.Types,
     uOps.Topic,
     uOps.Domain,
     uOps.DeadlineTimer,
     uOps.Transport.SendDataHandler,
     uOps.Transport.SendDataHandlerFactory,
     uOps.Transport.ReceiveDataHandler,
     uOps.Transport.ReceiveDataHandlerFactory,
     uOps.SerializableFactory,
     uOps.OPSObjectFactory,
     uOps.BasicError,
     uOps.OPSConfig,
     uOps.OpsObject,
     uOps.OpsMessage;

//#include "ThreadPool.h"
//#include "Runnable.h"
//#include "IOService.h"
//#include "DeadlineTimer.h"
//#include "ErrorService.h"
//#include "ParticipantInfoData.h"
//#include "ParticipantInfoDataListener.h"

type
//	//Forward declaration..
//  class Publisher;

	TParticipant = class(TObject) //  : Runnable, Listener<int>
  private
//		friend class Subscriber;
//		friend class Publisher;
//		friend class ParticipantInfoDataListener;
  public
    class function getInstance(domainID : string) : TParticipant; overload;
    class function getInstance(domainID : string; participantID : string) : TParticipant; overload;
    class function getInstance(domainID : string; participantID : string; configFile : string) : TParticipant; overload;

    // Report an error via all participants ErrorServices or the static ErrorService if it exists
    // Takes ownership over given TError instance
    class procedure ReportStaticError(error : TError);

//		//Create a Topic for subscribing or publishing on ParticipantInfoData
//		ops::Topic createParticipantInfoTopic();
//
//		//Get the name that this participant has set in its ParticipantInfoData
//		std::string getPartInfoName() {
//			return partInfoData.name;
//		}

    // Add a SerializableFactory which has support for data types (i.e. OPSObject derivatives you want this Participant to understand)
    // Takes over ownership of the object and it will be deleted with the participant
    procedure addTypeSupport(typeSupport : TSerializableFactory);

    // Create a Topic from the ops config. See config below.
    // Returns a reference to the internal storage
    function createTopic(name : string) : TTopic;

//		void run();

    // Make this participant report an Error, which will be delivered to all ErrorService listeners
    // Takes ownership over given TError instance
    procedure ReportError(error : TError);

//		///Deadline listener callback
//		void onNewEvent(Notifier<int>* sender, int message);
//
//		///Cleans up ReceiveDataHandlers
//		//void cleanUpReceiveDataHandlers();
//
//		///Get a pointer to the underlying IOService.
//		//TODO: private?
//		IOService* getIOService()
//		{
//			return ioService;
//		}

    // Returns a reference to the internal instance
    function getConfig : TOPSConfig;

//		ErrorService* getErrorService()
//		{
//			return errorService;
//		}
//
//		// A static error service that user could create, by calling getStaticErrorService(), and connect to.
//		// If it exist, "reportStaticError()" will use this instead of using all participants errorservices
//		// which leads to duplicated error messages when several participants exist.
//		// This static errorservice also has the advantage that errors during Participant creation can be logged.
//		static ErrorService* getStaticErrorService();

    // Returns a reference to the internal instance
    function getDomain : TDomain;

    // Get a reference to the data type factory used by this Participant.
    function getObjectFactory : TOPSObjectFactory;

    destructor Destroy; override;

		// Should only be used by Publishers
		function getSendDataHandler(top : TTopic) : TSendDataHandler;
		procedure releaseSendDataHandler(top : TTopic);

    // Should only be used by Subscribers
    function getReceiveDataHandler(top : TTopic) : TReceiveDataHandler;
    procedure releaseReceiveDataHandler(top : TTopic);

  private
    // The id of this participant, must be unique in process
    FParticipantID : string;

    // The domainID for this Participant
    FDomainID : string;

    // Objects on loan from OPSConfig
    FConfig : TOPSConfig;
    FDomain : TDomain;

//		///The IOService used for this participant, it handles communication and timers for all receivers, subscribers and member timers of this Participant.
//		IOService* ioService;

//		///The ErrorService
//		ErrorService* errorService;
//
//		///The threadPool drives ioService. By default Participant use a SingleThreadPool i.e. only one thread drives ioService.
//		ThreadPool* threadPool;
//
//		///A timer that fires with a certain periodicity, it keeps this Participant alive in the system by publishing ParticipantInfoData
//		DeadlineTimer* aliveDeadlineTimer;
//
//		//------------------------------------------------------------------------
//		///A publisher of ParticipantInfoData
//		Publisher* partInfoPub;
//
//		///The ParticipantInfoData that partInfoPub will publish periodically
//		ParticipantInfoData partInfoData;
//		Lockable partInfoDataMutex;

//		//------------------------------------------------------------------------
//		///A listener and handler for ParticipantInfoData
//    ParticipantInfoDataListener* partInfoListener;

    //------------------------------------------------------------------------
    //
    FReceiveDataHandlerFactory : TReceiveDataHandlerFactory;
    FSendDataHandlerFactory : TSendDataHandlerFactory;

//		///Mutex for ioService, used to shutdown safely
//		Lockable serviceMutex;

//		///As long this is true, we keep on running this participant
//		bool keepRunning;
//
//		///The interval with which this Participant publishes ParticipantInfoData
//		__int64 aliveTimeout;

    // The data type factory used in this Participant.
    FObjectFactory : TOPSObjectFactory;

    // Constructor is private, instances are acquired through getInstance()
    constructor Create(domainID : string; participantID : string; configFile : string);

    // Called from SendDataHandlerFactory when UDP topics should be connected/disconnected
    // to/from the participant info data listener
    procedure OnUdpConnectDisconnectProc(top : TTopic; sdh : TSendDataHandler; connect : Boolean);

//		///Remove this instance from the static instance map
//		void RemoveInstance();
//
//		//Visible to friends only
//		void setUdpTransportInfo(std::string ip, int port);
//		bool hasPublisherOn(std::string topicName);
//

  end;

implementation

uses
  SysUtils,
  uOps.Exceptions;

var
  // By Singelton, one Participant per 'domainId + participantID'
  gInstances : TDictionary<string,TParticipant>;
  gMutex : TMutex;

// -----------------------------------------------------------------------------

class function TParticipant.getInstance(domainID : string) : TParticipant;
begin
	Result := TParticipant.getInstance(domainID, 'DEFAULT_PARTICIPANT');
end;

class function TParticipant.getInstance(domainID : string; participantID : string) : TParticipant;
begin
	Result := TParticipant.getInstance(domainID, participantID, '');
end;

class function TParticipant.getInstance(domainID : string; participantID : string; configFile : string) : TParticipant;
var
  key : string;
begin
  Result := nil;
  gMutex.Acquire;
  try
    key := domainID + '::' + participantID;
    if not gInstances.ContainsKey(key) then begin
      try
        Result := TParticipant.Create(domainID, participantID, configFile);
        gInstances.Add(key, Result);
      except
        on ex : EConfigException do begin
          ReportStaticError(
            TBasicError.Create('Participant', 'Participant', ex.ToString()));
        end;
        on ex : ECommException do begin
          ReportStaticError(
            TBasicError.Create('Participant', 'Participant', ex.ToString()));
        end;
        else begin
          ReportStaticError(
            TBasicError.Create('Participant', 'Participant', 'Unknown Exception'));
        end;
      end;
    end else begin
      Result := gInstances.Items[key];
    end;
  finally
    gMutex.Release;
  end;
end;

// -----------------------------------------------------------------------------

// Report an error via all participants ErrorServices or the static ErrorService if it exists
// Takes ownership over given TError instance
class procedure TParticipant.ReportStaticError(error : TError);
begin
///TODO
///  don't forget to delete error instance
  FreeAndNil(error);
end;

// Make this participant report an Error, which will be delivered to all ErrorService listeners
// Takes ownership over given TError instance
procedure TParticipant.ReportError(error : TError);
begin
///TODO
///  don't forget to delete error instance
  FreeAndNil(error);
end;

// -----------------------------------------------------------------------------

// Constructor is private, instances are acquired through getInstance()
constructor TParticipant.Create(domainID : string; participantID : string; configFile : string);
begin
  inherited Create;
  FDomainID := domainID;
  FParticipantID := participantID;

  // Read configFile
  //Should trow?
  if configFile = '' then begin
    // This gets a reference to a singleton instance and should NOT be deleted.
    // It may be shared between several Participants.
    FConfig := TOPSConfig.getConfig;
  end else begin
    // This gets a reference to a unique instance and should eventually be deleted.
    // Note however that the getDomain() call below returns a reference to an
    // object internally in config.
    FConfig := TOPSConfig.getConfig(configFile);
  end;
  if not Assigned(FConfig) then begin
    raise ECommException.Create('No config on rundirectory');
  end;

  // Get the domain from config. Note should not be deleted, owned by config.
  FDomain := FConfig.getDomain(domainID);
  if not Assigned(FDomain) then begin
    raise ECommException.Create('Domain "' + domainID + '" missing in config-file');
  end;

  // Create a factory instance for each participant
  FObjectFactory := TOPSObjectFactoryImpl.Create;

  FSendDataHandlerFactory := TSendDataHandlerFactory.Create(FDomain, OnUdpConnectDisconnectProc);
  FReceiveDataHandlerFactory := TReceiveDataHandlerFactory.Create;



end;

destructor TParticipant.Destroy;
begin
  ///TODO Extract Self from gInstances ???


  FreeAndNil(FReceiveDataHandlerFactory);
  FreeAndNil(FSendDataHandlerFactory);
  FreeAndNil(FObjectFactory);

  TOPSConfig.releaseConfig(FConfig);
  inherited;
end;

// -----------------------------------------------------------------------------

function TParticipant.getSendDataHandler(top : TTopic) : TSendDataHandler;
begin
  Result := FSendDataHandlerFactory.getSendDataHandler(top);

  ///TODO add to partinfodata
end;

procedure TParticipant.releaseSendDataHandler(top : TTopic);
begin
  FSendDataHandlerFactory.releaseSendDataHandler(top);

  ///TODO remove from partinfodata
end;

function TParticipant.getReceiveDataHandler(top : TTopic) : TReceiveDataHandler;
begin
  Result := FReceiveDataHandlerFactory.getReceiveDataHandler(top, FDomain, FObjectFactory);

//		if (result) {
//			SafeLock lock(&partInfoDataMutex);
//			//Need to add topic to partInfoData.subscribeTopics (TODO ref count if same topic??)
//            partInfoData.subscribeTopics.push_back(TopicInfoData(top));
//		}
//		return result;
end;

procedure TParticipant.releaseReceiveDataHandler(top : TTopic);
begin
  FReceiveDataHandlerFactory.releaseReceiveDataHandler(top);

//		SafeLock lock(&partInfoDataMutex);
//		// Remove topic from partInfoData.subscribeTopics (TODO the same topic, ref count?)
//		std::vector<TopicInfoData>::iterator it;
//		for (it = partInfoData.subscribeTopics.begin(); it != partInfoData.subscribeTopics.end(); it++) {
//			if (it->name == top.getName()) {
//				partInfoData.subscribeTopics.erase(it);
//				break;
//			}
//		}
end;

function TParticipant.getObjectFactory : TOPSObjectFactory;
begin
  Result := FObjectFactory;
end;

// Add a SerializableFactory which has support for data types (i.e. OPSObject derivatives you want this Participant to understand)
// Takes over ownership of the object and it will be deleted with the participant
procedure TParticipant.addTypeSupport(typeSupport : TSerializableFactory);
begin
  FObjectFactory.Add(typeSupport);
end;

// Create a Topic from the ops config. See config below.
function TParticipant.createTopic(name : string) : TTopic;
begin
  Result := FDomain.getTopic(AnsiString(name));
  Result.ParticipantID := FParticipantID;
  Result.DomainID := FDomainID;
//  Result.participant = this;
end;

// Called from SendDataHandlerFactory when UDP topics should be connected/disconnected
// to/from the participant info data listener
procedure TParticipant.OnUdpConnectDisconnectProc(top : TTopic; sdh : TSendDataHandler; connect : Boolean);
begin
  if connect then begin
///TODO
  end else begin
///TODO
  end;
end;

function TParticipant.getDomain : TDomain;
begin
  Result := FDomain;
end;

function TParticipant.getConfig : TOPSConfig;
begin
  Result := FConfig;
end;

initialization
  gInstances := TDictionary<string,TParticipant>.Create;
  gMutex := TMutex.Create;

finalization
  FreeAndNil(gMutex);
  FreeAndNil(gInstances);

end.

