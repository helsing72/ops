/**
* 
* Copyright (C) 2006-2009 Anton Gravestam.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.

* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef ops_ParticipantH
#define	ops_ParticipantH

#include <string>
#include "ThreadPool.h"
#include "Runnable.h"
#include "IOService.h"
#include "SerializableFactory.h"
#include <map>
#include "Topic.h"
#include "OPSConfig.h"
#include "OPSObjectFactory.h"
#include "DeadlineTimer.h"
#include "Error.h"
#include "ErrorService.h"
#include "ParticipantInfoData.h"
#include "ParticipantInfoDataListener.h"
#include "SendDataHandler.h"



namespace ops
{
	//Forward declaration..
	class ReceiveDataHandler;
	class ReceiveDataHandlerFactory;
	class SendDataHandlerFactory;
	class Domain;
    class Publisher;

	class Participant : Runnable, Listener<int>
	{
		friend class Subscriber;
		friend class Publisher;
		friend class ReceiveDataHandlerFactory;
		friend class SendDataHandlerFactory;
		friend class ParticipantInfoDataListener;
	public:

		///By Singelton, one Participant per participantID
		static std::map<std::string, Participant*> instances;
		static Participant* getInstance(std::string domainID);
		static Participant* getInstance(std::string domainID, std::string participantID);
		static Participant* getInstance(std::string domainID, std::string participantID, std::string configFile);
		
		//Report an error via all participants ErrorServices or the static ErrorService if it exists
		static void reportStaticError(Error* err);

		//Create a Topic for subscribing or publishing on ParticipantInfoData
		ops::Topic createParticipantInfoTopic();

		//Add a SerializableFactory which has support for data types (i.e. OPSObject derivatives you want this Participant to understand)
		//Takes over ownership of the object and it will be deleted with the participant
		void addTypeSupport(ops::SerializableFactory* typeSupport);

		//Create a From the ops config. See config below.
		Topic createTopic(std::string name);

		void run();

		//Make this participant report an Error, which will be delivered to all ErrorService listeners
		void reportError(Error* err);

		///Deadline listener callback
		void onNewEvent(Notifier<int>* sender, int message);
		
		///Cleans up ReceiveDataHandlers
		//void cleanUpReceiveDataHandlers();

		///Get a pointer to the underlying IOService.
		//TODO: private?
		IOService* getIOService()
		{
			return ioService;
		}
		OPSConfig* getConfig()
		{
			return config;
		}
		
		ErrorService* getErrorService()
		{
			return errorService;
		}

		// A static error service that user could create, by calling getStaticErrorService(), and connect to. 
		// If it exist, "reportStaticError()" will use this instead of using all participants errorservices
		// which leads to duplicated error messages when several participants exist.
		// This static errorservice also has the advantage that errors during Participant creation can be logged.
		static ErrorService* getStaticErrorService();

		Domain* getDomain()
		{
			return domain;
		}

		///Get a pointer to the data type factory used in this Participant. 
		//TODO: Rename?
		OPSObjectFactory* getObjectFactory()
		{
			return objectFactory;
		}

		///Deprecated, use getErrorService()->addListener instead. Add a listener for OPS core reported Errors
		void addListener(Listener<Error*>* listener);
		///Deprecated, use getErrorService()->removeListener instead. Remove a listener for OPS core reported Errors
		void removeListener(Listener<Error*>* listener);

		//TODO: Review
		~Participant();

	private:

		///Constructor is private instance are acquired through getInstance()
		Participant(std::string domainID_, std::string participantID_, std::string configFile_);

		///Remove this instance from the static instance map
		void RemoveInstance();

		///The IOService used for this participant, it handles communication and timers for all receivers, subscribers and member timers of this Participant.
		IOService* ioService;
		OPSConfig* config;

		///The ErrorService
		ErrorService* errorService;

		///The threadPool drives ioService. By default Participant use a SingleThreadPool i.e. only one thread drives ioService.
		ThreadPool* threadPool;

		///A timer that fires with a certain periodicity, it keeps this Participant alive in the system by publishing ParticipantInfoData
		DeadlineTimer* aliveDeadlineTimer;

		//------------------------------------------------------------------------
		///A publisher of ParticipantInfoData
		Publisher* partInfoPub;
                
		///The ParticipantInfoData that partInfoPub will publish periodically
		ParticipantInfoData partInfoData;
		Lockable partInfoDataMutex;

		//Visible to friends only
		void setUdpTransportInfo(std::string ip, int port);
		bool hasPublisherOn(std::string topicName);

		Domain* domain;		

		//------------------------------------------------------------------------
		///A listener and handler for ParticipantInfoData
        ParticipantInfoDataListener* partInfoListener;

		//------------------------------------------------------------------------
		//
		ReceiveDataHandlerFactory* receiveDataHandlerFactory;
		SendDataHandlerFactory* sendDataHandlerFactory;

		//Visible to friends only
		//TODO: Deprecate and delegate to receiveDataHandlerFactory???
		ReceiveDataHandler* getReceiveDataHandler(Topic top);
		void releaseReceiveDataHandler(Topic top);

		///Visible to friends only
		//TODO: Deprecate and delegate to sendDataHandlerFactory???
		SendDataHandler* getSendDataHandler(Topic top);
		void releaseSendDataHandler(Topic top);		
		
		///Mutex for ioService, used to shutdown safely
		Lockable serviceMutex;

		///The domainID for this Participant
		std::string domainID;
		///The id of this participant, must be unique in process
		std::string participantID;

		///As long this is true, we keep on running this participant
		bool keepRunning;

		///The interval with which this Participant publishes ParticipantInfoData
		__int64 aliveTimeout;

		///The data type factory used in this Participant. 
		OPSObjectFactory* objectFactory;

		///Static Mutex used by factory methods getInstance()
		static Lockable creationMutex;

	};

}
#endif
