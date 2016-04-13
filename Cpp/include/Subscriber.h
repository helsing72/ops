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

#ifndef ops_SubscriberH
#define	ops_SubscriberH

#include "OPSTypeDefs.h"
#include "Topic.h"
#include "Lockable.h"
#include "OPSObject.h"
#include "DataNotifier.h"
#include "DeadlineMissedListener.h"
#include "FilterQoSPolicy.h"
#include <list>
#include <deque>
#include "ThreadPool.h"
#include "Listener.h"
#include "OPSMessage.h"
#include "ReceiveDataHandler.h"
#include "DeadlineTimer.h"
//#include <boost/asio.hpp>
//#include <boost/date_time/posix_time/posix_time.hpp>
#include "OPSExport.h"

// Forward declarations
namespace boost
{
    class mutex;
    class condition_variable;
}

namespace ops
{

    class OPS_EXPORT Subscriber : public DataNotifier, public Lockable, public Listener<OPSMessage*>, public Listener<int>
    {
    public:
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
        void setDeadlineQoS(__int64 deadlineT);
        __int64 getDeadlineQoS();

        void addFilterQoSPolicy(FilterQoSPolicy* fqos);
        void removeFilterQoSPolicy(FilterQoSPolicy* fqos);

        __int64 getTimeBasedFilterQoS();
        ///Sets the minimum time separation between to consecutive messages.
        ///Received messages in between will be ignored by this Subscriber
        void setTimeBasedFilterQoS(__int64 timeBaseMinSeparationMillis);

        ///Returns a copy of this subscribers Topic.
        Topic getTopic();

        ///Waits for new data to arrive or timeout.
        ///Returns: true if new data (i.e. unread data) exist.
        bool waitForNewData(int timeout);

        ///Checks if new data exist (same as 'waitForNewData(0)' but faster) 
        ///Returns: true if new data (i.e. unread data) exist.
        bool newDataExist() {
            return hasUnreadData;
        }

        ///Depricated since OPS4
        //int getThreadPolicy();
        ///Depricated since OPS4
        //void setThreadPolicy(int threadPolicy);

		///
        bool aquireMessageLock();
        void releaseMessageLock();

        ///Returns a reference to the latest received OPSMessage (including the latest data object).
        ///Clears the "new data" flag.
        ///NOTE: MessageLock should be hold while working with the message, to prevent a
        ///new incomming message to delete the current one while in use.
        OPSMessage* getMessage();

        ///Returns the number of reserved messages in the underlying ReceiveDataHandler
        ///This value is the total nr for this topic on this participant not only
        ///for this subscriber.
        int numReservedMessages()
        {
            return receiveDataHandler->numReservedMessages();
        }

        std::string getName();
        void setName(std::string name);

        //const static int SHARED_THREAD = 0;
        //const static int EXCLUSIVE_THREAD = 1;


        bool isDeadlineMissed();

        void setHistoryMaxSize(int s);
        std::deque<OPSMessage*> getHistory();

        ///Returns a reference the latest received data object.
        ///Clears the "new data" flag.
        ///NOTE: MessageLock should be hold while working with the data object, to prevent a 
        ///new incomming message to delete the current one while in use. 
        virtual OPSObject* getDataReference()
        {
            hasUnreadData = false;
            return data;
        }

        //Message listener callback
        void onNewEvent(Notifier<OPSMessage*>* sender, OPSMessage* message);

        //Deadline listener callback
        void onNewEvent(Notifier<int>* sender, int message);

    protected:
        void checkAndNotifyDeadlineMissed();

        OPSMessage* message;

        OPSObject* data;
        OPSObject* getData();
        bool firstDataReceived;
        bool hasUnreadData;

    private:

        ///The Participant to which this Subscriber belongs.
        Participant* participant;

        ///ReceiveDataHandler delivering new data samples to this Subscriber
        ReceiveDataHandler* receiveDataHandler;

        ///The Topic this Subscriber subscribes to.
        Topic topic;

        ///Name of this subscriber
        std::string name;

        ///Receiver side filters that will be applied to data from receiveDataHandler before delivery to application layer.
        std::list<FilterQoSPolicy*> filterQoSPolicies;

        std::deque<OPSMessage*> messageBuffer;
        unsigned int messageBufferMaxSize;
        void addToBuffer(OPSMessage* mess);

        Lockable filterQoSPolicyMutex;
        boost::mutex* newDataMutex;
        boost::condition_variable* newDataEvent;

        __int64 timeLastData;
        __int64 timeLastDataForTimeBase;
        __int64 timeBaseMinSeparationTime;
        __int64 deadlineTimeout;
        //int threadPolicy;
        //bool reliable;

        bool applyFilterQoSPolicies(OPSObject* o);
        bool applyKeyFilter(OPSObject* o);
        //void setData(OPSObject* data);

        void registerForDeadlineTimeouts();
        void cancelDeadlineTimeouts();

        bool deadlineMissed;
        void setDeadlineMissed(bool deadlineMissed);

        bool started;

        __int64 currentPulicationID;

        DeadlineTimer* deadlineTimer;
    };

}
#endif
