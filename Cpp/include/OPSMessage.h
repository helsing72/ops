/**
 *
 * Copyright (C) 2006-2009 Anton Gravestam.
 * Copyright (C) 2019 Lennart Andersson.
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

#include "OPSTypeDefs.h"
#include "OPSObject.h"
#ifndef OPSSLIM_NORESERVE
  #include "Reservable.h"
#endif
#include "ArchiverInOut.h"

namespace ops
{

    class OPSMessage : public OPSObject
#ifndef OPSSLIM_NORESERVE
	, public Reservable
#endif
    {
    public:
        OPSMessage() : 
#ifndef OPSSLIM_NORESERVE
        Reservable(),
#endif
        messageType(0),
        endianness(0),
        publisherPriority(),
        dataOwner(true),
        sourcePort(0),
        qosMask(0),
        publicationID(0),
        data(nullptr)
        {
			UNUSED(endianness)
			UNUSED(qosMask)
			TypeId_T typeName("ops.protocol.OPSMessage");
            OPSObject::appendType(typeName);
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
            if (dataOwner) {
                if (data) delete data;
            }
        }

    private:
        char messageType;			// Serialized (not used, always 0)
        char endianness;			//            (not used)
        char publisherPriority;		// Serialized (not used, always 0)
        bool dataOwner;
        int sourcePort;
		Address_T sourceIP;
        int64_t qosMask;
        int64_t publicationID;		// Serialized
		ObjectName_T publisherName;	// Serialized
        ObjectName_T topicName;		// Serialized
        ObjectKey_T topLevelKey;	// Serialized (not used, empty string)
		Address_T address;			// Serialized (not used, empty string)
        OPSObject* data;			// Serialized

		// Hide these since we don't support Clone of an OPSMessage()
		virtual OPSObject* clone() { return nullptr; }
		void fillClone(OPSMessage* obj) const { UNUSED(obj); }
	
	public:
        int64_t getPublicationID()
        {
            return publicationID;
        }

        void setPublicationID(int64_t pubID)
        {
            publicationID = pubID;
        }

		ObjectName_T getPublisherName()
        {
            return publisherName;
        }

        void setPublisherName(ObjectName_T pubName)
        {
            publisherName = pubName;
        }

        ObjectName_T getTopicName()
        {
            return topicName;
        }

        void setTopicName(ObjectName_T topName)
        {
            topicName = topName;
        }

        void setData(OPSObject* d)
        {
			if (dataOwner) {
				if (data && (data != d)) delete data;
			}
            data = d;
        }

        OPSObject* getData()
        {
            return data;
        }

		void setSource(Address_T addr, int port)
		{
			sourceIP = addr;
			sourcePort = port;
		}

		void getSource(Address_T& addr, int& port)
		{
			addr = sourceIP;
			port = sourcePort;
		}

        void serialize(ArchiverInOut* archive) override
        {
            OPSObject::serialize(archive);
			
			// Can't change/addto these without breaking compatbility
            archive->inout("messageType", messageType);
            archive->inout("publisherPriority", publisherPriority);
            archive->inout("publicationID", publicationID);
            archive->inout("publisherName", publisherName);
            archive->inout("topicName", topicName);
            archive->inout("topLevelKey", topLevelKey);
            archive->inout("address", address);
            data = (OPSObject*) archive->inout("data", data);

        }
    };

}
#endif
