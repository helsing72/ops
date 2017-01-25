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

#ifndef ops_ReceiveDataHandler_h
#define ops_ReceiveDataHandler_h

#include <string>
#include <map>
#include "Topic.h"
#include "ByteBuffer.h"
#include "OPSConstants.h"
#include "Receiver.h"
#include "OPSMessage.h"
#include "Notifier.h"
#include "Listener.h"
#include <iostream>
#include "BytesSizePair.h"
#include "ReferenceHandler.h"
#include "OPSExport.h"


namespace ops
{	//Forward declaration
	class Participant;
	
	class OPS_EXPORT ReceiveDataHandler : public Notifier<OPSMessage*>, Listener<BytesSizePair>
	{
	public:
		
		///Constructor.
		ReceiveDataHandler(Topic top, Participant* part);
		///Destructor
		virtual ~ReceiveDataHandler();

		bool aquireMessageLock();
		void releaseMessageLock();

		void stop();

		// We need to act on these localy
		// overridden from Notifier<OPSMessage*>
		void addListener(Listener<OPSMessage*>* listener);
		void removeListener(Listener<OPSMessage*>* listener);

		int numReservedMessages()
		{
			return messageReferenceHandler.size();
		}

		int getSampleMaxSize()
        {
            return sampleMaxSize;
        }

		Receiver* getReceiver()
		{
			return receiver;
		}

	protected:
		///Override from Listener
		///Called whenever the receiver has new data.
		void onNewEvent(Notifier<BytesSizePair>* sender, BytesSizePair byteSizePair);
		void calculateAndSetSpareBytes(ByteBuffer &buf, int segmentPaddingSize);

	private:
		///The receiver used for this topic. 
		Receiver* receiver;

		///Preallocated MemoryMap for receiving data
		MemoryMap memMap;
		int sampleMaxSize;

		//The Participant to which this ReceiveDataHandler belongs.
		Participant* participant;

		///Current OPSMessage, valid until next sample arrives.
		OPSMessage* message;

		///The accumulated size in bytes of the current message
		int currentMessageSize;

		Lockable messageLock;

		///ReferenceHandler that keeps track of object created on reception and deletes them when no one is interested anymore.
		ReferenceHandler messageReferenceHandler;
		
		int expectedSegment;
		bool firstReceived;

		////How many subscribers use this instance. This will be used to check if it is ok to delete this ReceiveDataHandler.
		//int reservations;
	};
	
}
#endif
