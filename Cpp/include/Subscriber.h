/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
 * Copyright (C) 2018 Lennart Andersson.
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

#pragma once

#include <list>
#include <deque>

#include "OPSTypeDefs.h"
#include "Topic.h"
#include "Lockable.h"
#include "OPSObject.h"
#include "DataNotifier.h"
#include "DeadlineMissedListener.h"
#include "FilterQoSPolicy.h"
#include "Listener.h"
#include "OPSMessage.h"
#include "ReceiveDataHandler.h"
#include "DeadlineTimer.h"
#include "OPSExport.h"
#include "PubIdChecker.h"
#include "DebugHandler.h"

#include <mutex>
#include <condition_variable>

namespace ops
{

    class OPS_EXPORT Subscriber : public DataNotifier, public Lockable, public Listener<OPSMessage*>, public Listener<int>,
		protected Listener<ConnectStatus>, public Notifier<ConnectStatus>
#ifdef OPS_ENABLE_DEBUG_HANDLER
		, DebugNotifyInterface
#endif
	{
    public:
        //
        // Methods marked with "(CB)" can be used from the callback.
        // Other methods should not be used from the callback.
        //

        Subscriber(Topic t);
        virtual ~Subscriber();

        ///Starts communication.
        void start();

        ///Stops communication, unsubscribe this subscriber to data.
        void stop();

        DeadlineMissedEvent deadlineMissedEvent;

        ///Sets the deadline timout for this subscriber.
        ///If no message is received within deadline,
        ///listeners to deadlineMissedEvent will be notified
        void setDeadlineQoS(int64_t deadlineT);
        int64_t getDeadlineQoS();

        void addFilterQoSPolicy(FilterQoSPolicy* fqos);
        void removeFilterQoSPolicy(FilterQoSPolicy* fqos);

        int64_t getTimeBasedFilterQoS();                                    // (CB)
        ///Sets the minimum time separation between to consecutive messages.
        ///Received messages in between will be ignored by this Subscriber
        void setTimeBasedFilterQoS(int64_t timeBaseMinSeparationMillis);    // (CB)

        ///Returns a copy of this subscribers Topic.
        Topic getTopic();                                                   // (CB)

        ///Waits for new data to arrive or timeout.
        ///Returns: true if new data (i.e. unread data) exist.
        bool waitForNewData(int timeoutMs);

        ///Checks if new data exist (same as 'waitForNewData(0)' but faster) 
        ///Returns: true if new data (i.e. unread data) exist.
        bool newDataExist() {                                               // (CB)
            return hasUnreadData;
        }

        ///Acquires/Releases the MessageLock.
        ///NOTE: The MessageLock is held by the subscriber while in the "New Message" callback
        bool aquireMessageLock();
        void releaseMessageLock();

        ///Returns a reference to the latest received OPSMessage (including the latest data object).
        ///Clears the "new data" flag.
        ///NOTE: MessageLock should be held while working with the message, to prevent a
        ///new incomming message to delete the current one while in use.
        OPSMessage* getMessage();                                           // (CB)

        ///Returns the number of reserved messages in the underlying ReceiveDataHandler
        ///This value is the total nr for this topic on this participant not only
        ///for this subscriber.
        int numReservedMessages()                                           // (CB)
        {
            return receiveDataHandler->numReservedMessages();
        }

        ObjectName_T getName();                                             // (CB)
        void setName(ObjectName_T name);                                    // (CB)

        bool isDeadlineMissed();

        void setHistoryMaxSize(int s);
        std::deque<OPSMessage*> getHistory();

        ///Returns a reference the latest received data object.
        ///Clears the "new data" flag.
        ///NOTE: MessageLock should be held while working with the data object, to prevent a
        ///new incomming message to delete the current one while in use. 
        virtual OPSObject* getDataReference()                               // (CB)
        {
            hasUnreadData = false;
            return data;
        }

        //Message listener callback
        void onNewEvent(Notifier<OPSMessage*>* sender, OPSMessage* message);

        //Deadline listener callback
        void onNewEvent(Notifier<int>* sender, int message);

		//Default NULL. Create a PublicationIdChecker if you want OPS to perform Publication Id checking.
		//The check is performed before any QoS filtering, so it sees all messages.
		//Add listerner(s) to the checker and you will be notified when:
		// - A new publisher is detected (Ip & Port of publisher is used)
		// - A Sequence Error is detected for an existing publisher
		PublicationIdChecker* pubIdChecker;

	protected:
        void checkAndNotifyDeadlineMissed();

        OPSMessage* message;

        OPSObject* data;
        OPSObject* getData();
        bool firstDataReceived;
        bool hasUnreadData;

		// Called from ReceiveDataHandler (TCPClient)
		virtual void onNewEvent(Notifier<ConnectStatus>* sender, ConnectStatus arg)
		{
			UNUSED(sender);
			// Forward status to all connected
			notifyNewEvent(arg);
		}

	private:
        ///The Participant to which this Subscriber belongs.
        Participant* participant;

        ///ReceiveDataHandler delivering new data samples to this Subscriber
        ReceiveDataHandler* receiveDataHandler;

        ///The Topic this Subscriber subscribes to.
        Topic topic;

        ///Name of this subscriber
		ObjectName_T name;

        ///Receiver side filters that will be applied to data from receiveDataHandler before delivery to application layer.
        std::list<FilterQoSPolicy*> filterQoSPolicies;

        std::deque<OPSMessage*> messageBuffer;
        unsigned int messageBufferMaxSize;
        void addToBuffer(OPSMessage* mess);

        Lockable filterQoSPolicyMutex;
		std::mutex newDataMutex;
		std::condition_variable newDataEvent;

        int64_t timeLastData;
        int64_t timeLastDataForTimeBase;
        int64_t timeBaseMinSeparationTime;
        int64_t deadlineTimeout;

        bool applyFilterQoSPolicies(OPSObject* o);
        //bool applyKeyFilter(OPSObject* o);

        void registerForDeadlineTimeouts();
        void cancelDeadlineTimeouts();

        bool deadlineMissed;
        void setDeadlineMissed(bool deadlineMissed);

        bool started;

        DeadlineTimer* deadlineTimer;

#ifdef OPS_ENABLE_DEBUG_HANDLER
		volatile int64_t _dbgSkip;
		volatile int64_t _numReceived;
		Lockable _dbgLock;
		virtual void onRequest(opsidls::DebugRequestResponseData& req, opsidls::DebugRequestResponseData& resp);
#endif
	};

}
