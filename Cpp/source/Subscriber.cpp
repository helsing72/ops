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
#include "OPSTypeDefs.h"
#include "Subscriber.h"
#include "TimeHelper.h"
#include "Participant.h"
#include "BasicError.h" 
#ifdef USE_C11
#include <chrono>
#else
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition_variable.hpp>
#include "boost/date_time/local_time/local_time.hpp"
#endif


namespace ops
{

    Subscriber::Subscriber(Topic t) :
    firstDataReceived(false),
    hasUnreadData(false),
    topic(t),
    messageBufferMaxSize(1),
    timeBaseMinSeparationTime(0),
    deadlineTimeout(TimeHelper::infinite),
    deadlineMissed(false),
    started(false)
    {
        message = NULL;
        data = NULL;

        participant = Participant::getInstance(topic.getDomainID(), topic.getParticipantID());
        deadlineTimer = DeadlineTimer::create(participant->getIOService());
        
#ifndef USE_C11
        newDataMutex = new boost::mutex();
        newDataEvent = new boost::condition_variable;
#endif
        timeLastData = TimeHelper::currentTimeMillis();
    }

    Subscriber::~Subscriber()
    {
        listeners.clear();
        if (started)
        {
            stop();
        }
        delete deadlineTimer;

		while (messageBuffer.size() > 0) {
			messageBuffer.back()->unreserve();
			messageBuffer.pop_back();
		}
#ifndef USE_C11
        delete newDataMutex;
        delete newDataEvent;
#endif
	}

    void Subscriber::start()
    {
		if (started) return;

        receiveDataHandler = participant->getReceiveDataHandler(topic);
        receiveDataHandler->addListener(this);
        deadlineTimer->addListener(this);
        deadlineTimer->start(deadlineTimeout);
        started = true;
    }

    void Subscriber::stop()
    {
		if (!started) return;

		receiveDataHandler->aquireMessageLock();
        receiveDataHandler->removeListener(this);
        receiveDataHandler->releaseMessageLock();
		receiveDataHandler = NULL;
        participant->releaseReceiveDataHandler(topic);
        deadlineTimer->removeListener(this);
        deadlineTimer->cancel();
        started = false;
    }

    void Subscriber::onNewEvent(Notifier<OPSMessage*>* sender, OPSMessage* message)
    {
        UNUSED(sender);
        //Perform a number of checks on incomming data to be sure we want to deliver it to the application layer

        //Check that this message is delivered on the same topic as this Subscriber use
        if (message->getTopicName() != topic.getName())
        {
            // This is a normal case when several Topics use the same port
            return;
        }
        //Check that the type of the delivered data can be interpreted as the type we expect in this Subscriber
        else if (message->getData()->getTypeString().find(topic.getTypeID()) == std::string::npos)
        {
            BasicError err("Subscriber", "onNewEvent", 
				std::string("Received message with wrong data type for Topic: ") + topic.getName() +
				std::string("\nExpected type: ") + topic.getTypeID() + 
				std::string("\nGot type: ") + message->getData()->getTypeString());
            participant->reportError(&err);
            return;
        }

        //OK, we passed the basic checks, lets go on and filter on data content...

        OPSObject* o = message->getData();
        if (applyFilterQoSPolicies(o))
        {
            if (TimeHelper::currentTimeMillis() - timeLastDataForTimeBase > timeBaseMinSeparationTime || timeBaseMinSeparationTime == 0)
            {
                firstDataReceived = true;
                
                addToBuffer(message);
                this->message = message;
                data = o;

                // Notify all registered listeners
                notifyNewData();

                // Signal any waiting thread(s)
#ifdef USE_C11
				std::unique_lock<std::mutex> lock(newDataMutex);
				hasUnreadData = true;
				newDataEvent.notify_all();
				lock.unlock();
#else
                boost::unique_lock<boost::mutex> lock(*newDataMutex);
                hasUnreadData = true;
                newDataEvent->notify_all();
                lock.unlock();
#endif

				// Update deadline variables
                timeLastDataForTimeBase = TimeHelper::currentTimeMillis();
                timeLastData = TimeHelper::currentTimeMillis();
                setDeadlineMissed(false);

                deadlineTimer->start(deadlineTimeout);

                //cancelDeadlineTimeouts();
                //registerForDeadlineTimeouts();
            }
        }
    }

    void Subscriber::addToBuffer(OPSMessage* mess)
    {
        mess->reserve();
        messageBuffer.push_front(mess);
        while (messageBuffer.size() > messageBufferMaxSize)
        {
            messageBuffer.back()->unreserve();
            messageBuffer.pop_back();
        }
    }

    void Subscriber::setHistoryMaxSize(int s)
    {
        messageBufferMaxSize = s;
    }

    std::deque<OPSMessage*> Subscriber::getHistory()
    {
        return messageBuffer;
    }

    OPSObject* Subscriber::getData()
    {
        hasUnreadData = false;
        return data;
    }

    Topic Subscriber::getTopic()
    {
        return topic;
    }

    void Subscriber::addFilterQoSPolicy(FilterQoSPolicy* fqos)
    {
        SafeLock lock(&filterQoSPolicyMutex);
        filterQoSPolicies.push_back(fqos);
    }

    void Subscriber::removeFilterQoSPolicy(FilterQoSPolicy* fqos)
    {
        SafeLock lock(&filterQoSPolicyMutex);
        filterQoSPolicies.remove(fqos);
    }

    bool Subscriber::applyFilterQoSPolicies(OPSObject* obj)
    {
        SafeLock lock(&filterQoSPolicyMutex);
        bool ret = true;
        std::list<FilterQoSPolicy*>::iterator p;
        p = filterQoSPolicies.begin();
        while (p != filterQoSPolicies.end())
        {
            if (!(*p)->applyFilter(obj))
            {
                ret = false;
                break;
            }
            p++;
        }
       
        return ret;
    }

    void Subscriber::setDeadlineQoS(__int64 millis)
    {
		if (millis == 0) {
		    deadlineTimeout = TimeHelper::infinite;
		} else {
	        deadlineTimeout = millis;
		}
		cancelDeadlineTimeouts();	// Restart with new timeout
    }

    __int64 Subscriber::getDeadlineQoS()
    {
        return deadlineTimeout;
    }

    void Subscriber::checkAndNotifyDeadlineMissed()
    {
        if (isDeadlineMissed())
        {
            //printf("DeadlineMissed timeLastData = %d, currTime = %d, deadlineTimeout = %d\n", timeLastData, currTime, deadlineTimeout);
            deadlineMissedEvent.notifyDeadlineMissed();
            timeLastData = TimeHelper::currentTimeMillis();
        }
    }

    __int64 Subscriber::getTimeBasedFilterQoS()
    {
        return timeBaseMinSeparationTime;
    }

    void Subscriber::setTimeBasedFilterQoS(__int64 timeBaseMinSeparationMillis)
    {
        timeBaseMinSeparationTime = timeBaseMinSeparationMillis;
    }

    bool Subscriber::waitForNewData(int timeoutMs)
    {
        if (hasUnreadData) {
            return true;
        }

#ifdef USE_C11
		std::unique_lock<std::mutex> lock(newDataMutex);

		if (newDataEvent.wait_for(lock, std::chrono::milliseconds(timeoutMs), [this] { return this->newDataExist(); })) {
			return true;
		}
#else
		boost::unique_lock<boost::mutex> lock(*newDataMutex);

        boost::system_time time = boost::get_system_time();
        time += boost::posix_time::milliseconds(timeoutMs);

        if(newDataEvent->timed_wait(lock, time)) {
            return true;
        }
#endif
		return false;
    }

    std::string Subscriber::getName()
    {
        return name;
    }

    void Subscriber::setName(std::string name)
    {
        this->name = name;
    }

    bool Subscriber::isDeadlineMissed()
    {
        __int64 currTime = TimeHelper::currentTimeMillis();
        if (currTime - timeLastData > deadlineTimeout)
        {
            deadlineMissed = true;
            return deadlineMissed;
        }
        return false;
    }

    void Subscriber::setDeadlineMissed(bool deadlineMissed)
    {
        this->deadlineMissed = deadlineMissed;
    }

    void Subscriber::registerForDeadlineTimeouts()
    {
        deadlineTimer->addListener(this);
    }

    void Subscriber::cancelDeadlineTimeouts()
    {
        deadlineTimer->start(deadlineTimeout);
    }

    void Subscriber::onNewEvent(Notifier<int>* sender, int message)
    {
        UNUSED(sender);
        UNUSED(message);
        deadlineMissedEvent.notifyDeadlineMissed();
        deadlineTimer->start(deadlineTimeout);
    }

    bool Subscriber::aquireMessageLock()
    {
        return receiveDataHandler->aquireMessageLock();
    }

    void Subscriber::releaseMessageLock()
    {
        receiveDataHandler->releaseMessageLock();
    }

    OPSMessage* Subscriber::getMessage()
    {
        hasUnreadData = false;
        return message;
    }

}
