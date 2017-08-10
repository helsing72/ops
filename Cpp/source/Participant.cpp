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
#include <sstream>
#ifdef _WIN32
#include <process.h>
#include <winsock.h>
#else
#include <sys/types.h>
#include <unistd.h>
#endif
#include "OPSTypeDefs.h"
#include "Participant.h"
#include "SingleThreadPool.h"
//#include "MultiThreadPool.h"
#include "ReceiveDataHandler.h"
#include "ReceiveDataHandlerFactory.h"
#include "SendDataHandlerFactory.h"
#include "OPSObjectFactoryImpl.h"
#include "ConfigException.h"
#include "CommException.h"
#include "Publisher.h"
#include "BasicError.h"
#include "NetworkSupport.h"
#include "ThreadSupport.h"

namespace ops
{
	//static
	std::map<ParticipantKey_T, Participant*> Participant::instances;
	Lockable Participant::creationMutex;


	// --------------------------------------------------------------------------------
	// A static error service that user could create, by calling getStaticErrorService(), and connect to. 
	// If it exist, "reportStaticError()" will use this instead of using all participants errorservices
	// which leads to duplicated error messages when several participants exist.
	// This static errorservice also has the advantage that errors during Participant creation can be logged.
	static ErrorService* staticErrorService = NULL;

	ErrorService* Participant::getStaticErrorService()
	{
		SafeLock lock(&creationMutex);
		if (!staticErrorService) {
			staticErrorService = new ErrorService();
		}
		return staticErrorService;
	}


	// --------------------------------------------------------------------------------

	Participant* Participant::getInstance(ObjectName_T domainID_)
	{
		return getInstance(domainID_, "DEFAULT_PARTICIPANT");
	}

	Participant* Participant::getInstance(ObjectName_T domainID_, ObjectName_T participantID)
	{
		return getInstance(domainID_, participantID, "");
	}

	ParticipantKey_T getKey(ObjectName_T domainID_, ObjectName_T participantID)
	{
		ParticipantKey_T key = domainID_;
		key += "::";
		key += participantID;
		return key;
	}

	Participant* Participant::getInstance(ObjectName_T domainID_, ObjectName_T participantID, FileName_T configFile)
	{
		ParticipantKey_T key = getKey(domainID_, participantID);
		SafeLock lock(&creationMutex);
		if (instances.find(key) == instances.end()) {
			try
			{
				Participant* newInst = new Participant(domainID_, participantID, configFile);
				instances[key] = newInst;
			}
			catch(ops::ConfigException& ex)
			{
				ErrorMessage_T msg("Exception: ");
				msg += ex.what();
				BasicError err("Participant", "Participant", msg);
				reportStaticError(&err);
				return NULL;
			}
			catch (ops::exceptions::CommException& ex)
			{
				BasicError err("Participant", "Participant", ex.what());
				reportStaticError(&err);
				return NULL;
			}
			catch(...)
			{
				BasicError err("Participant", "Participant", "Unknown Exception");
				reportStaticError(&err);
				return NULL;
			}
		}
		return instances[key];
	}

	///Remove this instance from the static instance map
	void Participant::RemoveInstance()
	{
		ParticipantKey_T key = getKey(domainID, participantID);
		SafeLock lock(&creationMutex);
		instances.erase(key);		
	}


	Participant::Participant(ObjectName_T domainID_, ObjectName_T participantID_, FileName_T configFile_):
		ioService(NULL),
		config(NULL),
		errorService(NULL), 
		threadPool(NULL),
		aliveDeadlineTimer(NULL),
		partInfoPub(NULL),
		domain(NULL), 
		partInfoListener(NULL),
		receiveDataHandlerFactory(NULL),
		sendDataHandlerFactory(NULL),
		domainID(domainID_), 
		participantID(participantID_),
		keepRunning(true),
		aliveTimeout(1000),
		objectFactory(NULL)
	{	
		ioService = IOService::create();

		if(!ioService)
		{
			//Error, should never happen, throw?
            exceptions::CommException ex("No config on rundirectory");
			throw ex;
		}

		//Should trow?
		if (configFile_ == "") {
			// This gets a reference to a singleton instance and should NOT be deleted.
			// It may be shared between several Participants.
			config = OPSConfig::getConfig();
		} else {
			// This gets a reference to a unique instance and should eventually be deleted.
			// Note however that the getDomain() call below returns a reference to an
			// object internally in config.
			config = OPSConfig::getConfig(configFile_);
		}
		if(!config)
		{
			ops::ConfigException ex("No config on rundirectory?");
			throw ex;
		}

		//Get the domain from config. Note should not be deleted, owned by config.
		domain = config->getDomain(domainID);
		if(!domain) 
		{
			ExceptionMessage_T msg("Domain '");
			msg += domainID;
			msg += "' missing in config-file";
			exceptions::CommException ex(msg);
			throw ex;
		}

		//Create a factory instance for each participant
		objectFactory = new OPSObjectFactoryImpl();
		
		// Initialize static data in partInfoData (ReceiveDataHandlerFactory() will set some more fields)
		InternalString_T Name = GetHostName();
		std::ostringstream myStream;
#ifdef _WIN32
		myStream << Name << " (" << _getpid() << ")" << std::ends;
#else
		myStream << Name << " (" << getpid() << ")" << std::ends;
#endif
		Name = myStream.str().c_str();
		partInfoData.name = Name;
        partInfoData.languageImplementation = "C++";
        partInfoData.id = participantID;
        partInfoData.domain = domainID;

		//-----------Create delegate helper classes---
		errorService = new ErrorService();
		receiveDataHandlerFactory = new ReceiveDataHandlerFactory(this);
		sendDataHandlerFactory = new SendDataHandlerFactory(this);
		//--------------------------------------------

		//------------Create timer for periodic events-
		aliveDeadlineTimer = DeadlineTimer::create(ioService);
		aliveDeadlineTimer->addListener(this);
		//--------------------------------------------

		//------------Create thread pool--------------
		threadPool = new SingleThreadPool();
		//threadPool = new MultiThreadPool();
		threadPool->addRunnable(this);
		threadPool->start();
		//--------------------------------------------

		// Create the listener object for the participant info data published by participants on our domain.
		// The actual subscriber won't be created until some one needs it.
		// We use the information for topics with UDP as transport, to know the destination for UDP sends
		// ie. we extract ip and port from the information and add it to our McUdpSendDataHandler.
		partInfoListener = new ParticipantInfoDataListener(this);
	}

	Participant::~Participant()
	{
		// We assume that the user has deleted all publishers and subscribers connected to this Participant.
		// We also assume that the user has cancelled eventual deadlinetimers etc. connected to the ioService.
		// We also assume that the user has unreserved() all messages that he has reserved().

		// Remove this instance from the static instance map
		RemoveInstance();

		{
			SafeLock lock(&serviceMutex);
		
			// Indicate that shutdown is in progress
			keepRunning = false;

			// We have indicated shutdown in progress. Delete the partInfoData Publisher.
			// Note that this uses our sendDataHandlerFactory.
			if (partInfoPub) delete partInfoPub;
			partInfoPub = NULL;
		}

		// Stop the subscriber for partInfoData. This requires ioService to be running.
		// Note that this (the subscriber) uses our receiveDataHandlerFactory.
		if (partInfoListener) partInfoListener->prepareForDelete();

		// Now delete our send factory (TODO it does not cleanup correctly yet)
		if (sendDataHandlerFactory) delete sendDataHandlerFactory;
		sendDataHandlerFactory = NULL;

		// Our timer is required for ReceiveDataHandlers to be cleaned up so it shouldn't be stopped 
		// before receiveDataHandlerFactory is finished.
		// Wait until receiveDataHandlerFactory has no more cleanup to do
		while (!receiveDataHandlerFactory->cleanUpDone()) {
			TimeHelper::sleep(1);
		}

		// Now stop and delete our timer (NOTE requires ioService to be running).
		// If the timer is in the callback, the delete will wait for it to finish and then the object is deleted.
		if (aliveDeadlineTimer) delete aliveDeadlineTimer; 
		aliveDeadlineTimer = NULL;

		// Now time to delete our receive factory
		if (receiveDataHandlerFactory) delete receiveDataHandlerFactory;
		receiveDataHandlerFactory = NULL;

		// There should now not be anything left requiring the ioService to be running.

		// Then we request the IO Service to stop the processing (it's running on the threadpool).
		// The stop() call will not block, it just signals that we want it to finish as soon as possible.
		if (ioService) ioService->stop();

		// Now we delete the threadpool, which will wait for the thread(s) to finish 
		if (threadPool) delete threadPool;
		threadPool = NULL;

		// Now when the threads are gone, it's safe to delete the rest of our objects 
		if (partInfoListener) delete partInfoListener;
		if (objectFactory) delete objectFactory;
		if (errorService) delete errorService;
		// All objects connected to this ioservice should now be deleted, so it should be safe to delete it
		if (ioService) delete ioService;
	}

	ops::Topic Participant::createParticipantInfoTopic()
	{
///		ops::Topic infoTopic("ops.bit.ParticipantInfoTopic", 9494, "ops.ParticipantInfoData", domain->getDomainAddress());
		ops::Topic infoTopic("ops.bit.ParticipantInfoTopic", domain->getMetaDataMcPort(), "ops.ParticipantInfoData", domain->getDomainAddress());
		infoTopic.setLocalInterface(domain->getLocalInterface());
		infoTopic.setTimeToLive(domain->getTimeToLive());
		infoTopic.setDomainID(domainID);
		infoTopic.setParticipantID(participantID);
		infoTopic.setTransport(Topic::TRANSPORT_MC);
		
		return infoTopic;
	}

	// Report an error via the participants ErrorService
	void Participant::reportError(Error* err)
	{
		errorService->report(err);
	}

	// Report an error via all participants ErrorServices
	void Participant::reportStaticError(Error* err)
	{
		if (staticErrorService) {
			staticErrorService->report(err);

		} else {
			std::map<ParticipantKey_T, Participant*>::iterator it = instances.begin();
			while(it !=instances.end())
			{
				it->second->getErrorService()->report(err);
				++it;
			}
		}
	}

	// This will be called by our threadpool (started in the constructor())
	void Participant::run()
	{
		// Set name of current thread for debug purpose
		InternalString_T name("OPSP_");
		name += domainID;
		thread_support::SetThreadName(name.c_str());
		// Start our timer. Calls onNewEvent(Notifier<int>* sender, int message) on timeout
		aliveDeadlineTimer->start(aliveTimeout);
		ioService->run();	
	}

	// Called on aliveDeadlineTimer timeouts
	void Participant::onNewEvent(Notifier<int>* sender, int message)
	{
		UNUSED(sender);
		UNUSED(message);
		SafeLock lock(&serviceMutex);
		receiveDataHandlerFactory->cleanUpReceiveDataHandlers();

		if (keepRunning) {
			try {
				// Create the meta data publisher if user hasn't disabled it for the domain.
				// The meta data publisher is only necessary if we have topics using transport UDP.
				if ( (partInfoPub == NULL) && (domain->getMetaDataMcPort() > 0) )
				{
					partInfoPub = new Publisher(createParticipantInfoTopic());
				}
				if (partInfoPub) {
					SafeLock lock(&partInfoDataMutex);
					partInfoPub->writeOPSObject(&partInfoData);
				}
			} catch (std::exception& ex)
			{
				ErrorMessage_T errMessage;
				if (partInfoPub == NULL) {
					errMessage = "Failed to create publisher for ParticipantInfoTopic. Check localInterface and metaDataMcPort in configuration file.";
				} else {
					errMessage = "Failed to publish ParticipantInfoTopic data.";
				}
				errMessage += " Exception: ";
				errMessage += ex.what();
				BasicError err("Participant", "onNewEvent", errMessage);
				reportStaticError(&err);
			}
		}

		// Start a new timeout
		aliveDeadlineTimer->start(aliveTimeout);
	}

	void Participant::setUdpTransportInfo(Address_T ip, int port)
	{
		SafeLock lock(&partInfoDataMutex);
		partInfoData.ip = ip;
		partInfoData.mc_udp_port = port;
	}

	void Participant::addTypeSupport(ops::SerializableFactory* typeSupport)
	{
		objectFactory->add(typeSupport);
	}

	Topic Participant::createTopic(ObjectName_T name)
	{
		Topic topic = domain->getTopic(name);
		topic.setParticipantID(participantID);
		topic.setDomainID(domainID);
		topic.participant = this;		
		
		return topic;
	}

	///Deprecated, use getErrorService()->addListener instead. Add a listener for OPS core reported Errors
	void Participant::addListener(Listener<Error*>* listener)
	{
		errorService->addListener(listener);
	}

	///Deprecated, use getErrorService()->removeListener instead. Remove a listener for OPS core reported Errors
	void Participant::removeListener(Listener<Error*>* listener)
	{
		errorService->removeListener(listener);
	}

	bool Participant::hasPublisherOn(ObjectName_T topicName)
	{
		UNUSED(topicName);
		///TODO
		return true;
	}

	ReceiveDataHandler* Participant::getReceiveDataHandler(Topic top)
	{
		ReceiveDataHandler* result = receiveDataHandlerFactory->getReceiveDataHandler(top, this);
		if (result) {
			SafeLock lock(&partInfoDataMutex);
			//Need to add topic to partInfoData.subscribeTopics (TODO ref count if same topic??)
            partInfoData.subscribeTopics.push_back(TopicInfoData(top));
		}
		return result;
		
	}

	void Participant::releaseReceiveDataHandler(Topic top)
	{
		receiveDataHandlerFactory->releaseReceiveDataHandler(top, this);

		SafeLock lock(&partInfoDataMutex);
		// Remove topic from partInfoData.subscribeTopics (TODO the same topic, ref count?)
		std::vector<TopicInfoData>::iterator it;
		for (it = partInfoData.subscribeTopics.begin(); it != partInfoData.subscribeTopics.end(); ++it) {
			if (it->name == top.getName()) {
				partInfoData.subscribeTopics.erase(it);
				break;
			}
		}
	}

	SendDataHandler* Participant::getSendDataHandler(Topic top)
	{
		SendDataHandler* result = sendDataHandlerFactory->getSendDataHandler(top, this);
		if (result) {
			SafeLock lock(&partInfoDataMutex);
			//Need to add topic to partInfoData.subscribeTopics (TODO ref count if same topic??)
            partInfoData.publishTopics.push_back(TopicInfoData(top));
		}
		return result;
	}

	void Participant::releaseSendDataHandler(Topic top)
	{
		sendDataHandlerFactory->releaseSendDataHandler(top, this);

		SafeLock lock(&partInfoDataMutex);
		// Remove topic from partInfoData.publishTopics (TODO the same topic, ref count?)
		std::vector<TopicInfoData>::iterator it;
		for (it = partInfoData.publishTopics.begin(); it != partInfoData.publishTopics.end(); ++it) {
			if (it->name == top.getName()) {
				partInfoData.publishTopics.erase(it);
				break;
			}
		}
	}

}
