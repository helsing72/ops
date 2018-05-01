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

#ifndef ops_OPSObjectFactoryImpl_h
#define ops_OPSObjectFactoryImpl_h

#include "OPSTypeDefs.h"
#include "OPSMessage.h"
#include "Channel.h"
#include "Topic.h"
#include "Transport.h"
#include "DefaultOPSConfigImpl.h"
#include "Domain.h"
#include "ParticipantInfoData.h"
#ifdef OPS_ENABLE_DEBUG_HANDLER
	#include "ops/DebugRequestResponseData.h"
#endif

namespace ops
{

    class BuiltInFactory : public SerializableFactory
    {
    public:

        Serializable* create(const TypeId_T& type)
        {
            if (type == ("ops.protocol.OPSMessage"))
            {
                return new OPSMessage();
            }
            if (type == ("Topic"))
            {
                return new Topic();
            }
            if (type == ("Channel"))
            {
                return new Channel();
            }
            if (type == ("Transport"))
            {
                return new Transport();
            }
            if (type == ("DefaultOPSConfigImpl"))
            {
                return new DefaultOPSConfigImpl();
            }
            if (type == ("MulticastDomain"))
            {
                return new Domain();
            }
            if (type == ("Domain"))
            {
                return new Domain();
            }
            if (type == ("ops.ParticipantInfoData"))
            {
                return new ParticipantInfoData();
            }
#ifdef OPS_ENABLE_DEBUG_HANDLER
			if (type == ("ops.DebugRequestResponseData"))
			{
				return new DebugRequestResponseData();
			}
#endif
			return NULL;
        }
    };

    class OPSObjectFactoryImpl : public OPSObjectFactory
    {
    public:
        OPSObjectFactoryImpl()
        {
            add(new BuiltInFactory());
        }
    };

}
#endif
