/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
 * Copyright (C) 2018-2019 Lennart Andersson.
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

#include "Publisher.h"
#include "Participant.h"
#include "Domain.h"
#include "OPSArchiverOut.h"
#include "OPSConstants.h"
#include "TimeHelper.h"
#include "DataSegmentPool.h"

namespace ops
{
    Publisher::Publisher(Topic t) :
    topic(t),
    memMap(t.getSampleMaxSize() / OPSConstants::PACKET_MAX_SIZE + 1, OPSConstants::PACKET_MAX_SIZE, &DataSegmentAllocator::Instance()),
    currentPublicationID(0),
    name(""),
    key(""),
#ifdef OPS_ENABLE_DEBUG_HANDLER
	_dbgSkip(0),
#endif
    sendSleepTime(1),
    sleepEverySendPacket(100000)
    {
        participant = Participant::getInstance(topic.getDomainID(), topic.getParticipantID());
        sendDataHandler = participant->getSendDataHandler(topic);

        message.setPublisherName(name);
        message.setTopicName(topic.getName());
        message.setDataOwner(false);
		
		start();

#ifdef OPS_ENABLE_DEBUG_HANDLER
		participant->debugHandler.RegisterPub(this, topic.getName());
#endif
	}

    Publisher::~Publisher()
    {
		OPS_TRACE("Pub: Destructor()...\n");
#ifdef OPS_ENABLE_DEBUG_HANDLER
		participant->debugHandler.UnregisterPub(this, topic.getName());
		for (unsigned int i = 0; i < _replace.size(); i++) {
			if (_replace[i] != nullptr) { delete _replace[i]; }
		}
#endif
		stop();
		participant->releaseSendDataHandler(topic);
		OPS_TRACE("Pub: Destructor() finished\n");
	}

	void Publisher::start()
	{
		sendDataHandler->addListener(this);
		sendDataHandler->addPublisher(this, topic);
	}

	void Publisher::stop()
	{
		sendDataHandler->removeListener(this);
		sendDataHandler->removePublisher(this, topic);
	}

    Topic Publisher::getTopic() const
    {
        return this->topic;
    }

    void Publisher::setName(ObjectName_T name)
    {
        this->name = name;
    }

    void Publisher::setKey(ObjectKey_T key)
    {
        this->key = key;
    }

    ObjectKey_T Publisher::getKey() const
    {
        return this->key;
    }

	ObjectName_T Publisher::getName() const
    {
        return this->name;
    }

    void Publisher::writeOPSObject(OPSObject* obj)
    {
        write(obj);
    }

	void Publisher::write(OPSObject* data)
	{
#ifdef OPS_ENABLE_DEBUG_HANDLER
		SafeLock lck(&_dbgLock);
		if (_dbgSkip > 0) {
			_dbgSkip--;
		} else {
			if (_replace.size() > 0) {
				internalWrite(_replace[_replace.size()-1]);
				delete _replace[_replace.size() - 1];
				_replace.pop_back();
			} else {
				internalWrite(data);
			}
		}
	}

	void Publisher::internalWrite(OPSObject* data)
	{
#endif
		if (key != "")
        {
            data->setKey(key);
        }

        ByteBuffer buf(memMap);

        message.setData(data);

        message.setPublicationID(currentPublicationID);
        message.setPublisherName(name);

        buf.writeNewSegment();

        OPSArchiverOut archive(buf);

        archive.inout("message", &message);

        //If data has spare bytes, write them to the end of the buf
        if (message.getData()->spareBytes.size() > 0)
        {
            buf.WriteChars(&(message.getData()->spareBytes[0]), (int)message.getData()->spareBytes.size());
        }

        buf.finish();

        for (int i = 0; i < buf.getNrOfSegments(); i++)
        {
            int segSize = buf.getSegmentSize(i);
            bool sendOK = sendDataHandler->sendData(buf.getSegment(i), segSize, topic);
            if (!sendOK)
            {
                TimeHelper::sleep(sendSleepTime);
                sendDataHandler->sendData(buf.getSegment(i), segSize, topic);
            }
			else if ((i > 0) && (i % sleepEverySendPacket == 0))
            {
                TimeHelper::sleep(sendSleepTime);
            }
        }

		currentPublicationID++;
    }

#ifdef OPS_ENABLE_DEBUG_HANDLER
	void Publisher::onRequest(opsidls::DebugRequestResponseData& req, opsidls::DebugRequestResponseData& resp)
	{
		SafeLock lck(&_dbgLock);
		switch (req.Command) {
		case 1: // Request status
			break;
		case 2: // Enable
			_dbgSkip = (req.Param1 == 1) ? 0 : LLONG_MAX;
			break;
		case 3: // PubId
			currentPublicationID += req.Param1;
			break;
		case 4: // Skip next x sends
			_dbgSkip = req.Param1;
			break;
		case 5: // Send directly
			if (req.Objs.size() == 0) break;
			if (req.Objs[0] != nullptr) { internalWrite(req.Objs[0]); }
			break;
		case 6: // Send instead of next x ordinary
			// Free ev. objects stored since earlier 
			for (unsigned int i = 0; i < _replace.size(); i++) {
				if (_replace[i] != nullptr) { delete _replace[i]; }
			}
			_replace.clear();
			// Store backwards in _replace vector
			for (auto i = req.Objs.size(); i > 0; i--) {
				_replace.push_back(req.Objs[i-1]);
			}
			// We have now taken over ownership
			req.Objs.clear();
			break;
		default:;
			break;
		}

		// Fill in status
		resp.Enabled = _dbgSkip == 0;
		resp.Result1 = currentPublicationID;
		resp.Result2 = _dbgSkip;
		resp.Result3 = (_replace.size() > 0);
	}
#endif
}
