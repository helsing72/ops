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

#include "Publisher.h"
#include "Participant.h"
#include "Domain.h"
#include "OPSArchiverOut.h"
#include "OPSConstants.h"
#include "TimeHelper.h"

namespace ops
{
    Publisher::Publisher(Topic t) :
    topic(t),
    memMap(t.getSampleMaxSize() / OPSConstants::PACKET_MAX_SIZE + 1, OPSConstants::PACKET_MAX_SIZE),
    currentPublicationID(0),
    name(""),
    key(""),
#ifdef OPS_ENABLE_DEBUG_HANDLER
	_enabled(true),
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
#ifdef OPS_ENABLE_DEBUG_HANDLER
		participant->debugHandler.UnregisterPub(this, topic.getName());
#endif
		stop();
		participant->releaseSendDataHandler(topic);
	}

	void Publisher::start()
	{
		sendDataHandler->addPublisher(this);
	}

	void Publisher::stop()
	{
		sendDataHandler->removePublisher(this);
	}

    Topic Publisher::getTopic()
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

    ObjectKey_T Publisher::getKey()
    {
        return this->key;
    }

	ObjectName_T Publisher::getName()
    {
        return this->name;
    }

    void Publisher::writeOPSObject(OPSObject* obj)
    {
        write(obj);
    }

    void Publisher::write(OPSObject* data)
    {
        if (key != "")
        {
            data->setKey(key);
        }

        ByteBuffer buf(&memMap);

        message.setData(data);

        message.setPublicationID(currentPublicationID);
        message.setPublisherName(name);

        buf.writeNewSegment();

        OPSArchiverOut archive(&buf);

        archive.inout("message", &message);

        //If data has spare bytes, write them to the end of the buf
        if (message.getData()->spareBytes.size() > 0)
        {
            buf.WriteChars(&(message.getData()->spareBytes[0]), (int)message.getData()->spareBytes.size());
        }

        buf.finish();

#ifdef OPS_ENABLE_DEBUG_HANDLER
		if (_enabled)
#endif
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

        IncCurrentPublicationID();
    }

    void Publisher::IncCurrentPublicationID()
    {
        currentPublicationID++;
    }

#ifdef OPS_ENABLE_DEBUG_HANDLER
	void Publisher::onRequest(DebugRequestResponseData& req, DebugRequestResponseData& resp)
	{
		switch (req.Command) {
		case 1: // Request
			break;
		case 2: // Enable
			_enabled = (req.Param1 == 1);
			break;
		case 3: // PubId
			currentPublicationID += req.Param1;		// TODO thread safety
			break;
		case 4: // Skip
			///TODO
			break;
		case 5: // Send
			if (req.Objs.size() == 0) break;
			if (req.Objs[0]) write(req.Objs[0]);	// TODO thread safety
			break;
		}

		// Fill in status
		resp.Enabled = _enabled;
		resp.Result1 = currentPublicationID;
		resp.Result2 = 0;
		resp.Result3 = false;
	}
#endif
}
