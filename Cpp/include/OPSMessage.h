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

#ifndef OPSMessageH
#define OPSMessageH

#include <string>
#include "OPSObject.h"
#include "Reservable.h"
#include "ArchiverInOut.h"

namespace ops
{

    class OPSMessage : public OPSObject, public Reservable
    {
    public:

        OPSMessage() : Reservable(),
        messageType(0),
        endianness(0),
        publisherPriority(),
        sourcePort(0),
        qosMask(0),
        publicationID(0),
        dataOwner(true)
        {
            std::string typeName("ops.protocol.OPSMessage");
            OPSObject::appendType(typeName);
            data = NULL;

        }

        virtual void setDataOwner(bool ownership)
        {
            dataOwner = ownership;
        }

        virtual bool isDataOwner()
        {
            return dataOwner;
        }

        virtual ~OPSMessage()
        {
            //TODO: should this delete data???
            if (dataOwner)
            {
                if (data) delete data;
            }
        }

    private:
        char messageType;			// Serialized
        char endianness;
        char publisherPriority;		// Serialized
        bool dataOwner;
        int sourcePort;
		std::string sourceIP;
        __int64 qosMask;
        __int64 publicationID;		// Serialized
        std::string publisherName;	// Serialized
        std::string topicName;		// Serialized
        std::string topLevelKey;	// Serialized
        std::string address;		// Serialized
        OPSObject* data;			// Serialized

    public:

        __int64 getPublicationID()
        {
            return publicationID;
        }

        void setPublicationID(__int64 pubID)
        {
            publicationID = pubID;
        }

        std::string getPublisherName()
        {
            return publisherName;
        }

        void setPublisherName(std::string pubName)
        {
            publisherName = pubName;
        }

        std::string getTopicName()
        {
            return topicName;
        }

        void setTopicName(std::string topName)
        {
            topicName = topName;
        }

        void setData(OPSObject* d)
        {
            data = d;
        }

        OPSObject* getData()
        {
            return data;
        }

		void setSource(std::string addr, int port)
		{
			sourceIP = addr;
			sourcePort = port;
		}

		void getSource(std::string& addr, int& port)
		{
			addr = sourceIP;
			port = sourcePort;
		}

        void serialize(ArchiverInOut* archive)
        {
            OPSObject::serialize(archive);
			
			// Can't change/addto these without breaking compatbility
            archive->inout(std::string("messageType"), messageType);
            archive->inout(std::string("publisherPriority"), publisherPriority);
            archive->inout(std::string("publicationID"), publicationID);
            archive->inout(std::string("publisherName"), publisherName);
            archive->inout(std::string("topicName"), topicName);
            archive->inout(std::string("topLevelKey"), topLevelKey);
            archive->inout(std::string("address"), address);
            data = (OPSObject*) archive->inout(std::string("data"), data);

        }
    };

}


#endif
