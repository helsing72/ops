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

package ops.archiver;

import configlib.Serializable;
import configlib.SerializableCompositeFactory;
import configlib.SerializableFactory;
import ops.DefaultOPSConfigImpl;
import ops.Domain;
import ops.Topic;
import ops.Channel;
import ops.Transport;
import ops.protocol.OPSMessage;
import ops.ParticipantInfoData;
import ops.TopicInfoData;

/**
 *
 * @author angr
 */
public class OPSObjectFactory extends SerializableCompositeFactory
{
    static OPSObjectFactory instance = null;
    /**
     * Create singelton instance of OPSObjectFactory.
     * @return
     */
    public static OPSObjectFactory getInstance()
    {
        if(instance == null)
        {
            instance = new OPSObjectFactory();
            instance.add(new SerializableFactory() {

                public Serializable create(String type)
                {
                    if(type.equals("ops.protocol.OPSMessage"))
                    {
                        return new OPSMessage();
                    }
                    if (type.equals("ops.ParticipantInfoData"))
                    {
                        return new ParticipantInfoData();
                    }
                    if (type.equals("TopicInfoData"))
                    {
                        return new TopicInfoData();
                    }
                    if(type.equals("DefaultOPSConfigImpl"))
                    {
                        return new DefaultOPSConfigImpl();
                    }
                    if(type.equals("Domain"))
                    {
                        return new Domain();
                    }
                    if(type.equals("MulticastDomain"))
                    {
                        return new Domain();
                    }
                    if(type.equals("Topic"))
                    {
                        return new Topic();
                    }
                    if(type.equals("Channel"))
                    {
                        return new Channel();
                    }
                    if(type.equals("Transport"))
                    {
                        return new Transport();
                    }

                    return null;
                }
            });
        }
        return instance;
    }
    /**
     * Tries to construct the most specialized version of the given typeString
     */
    @Override
    public Serializable create(String typeString)
    {
        String[] types = typeString.split(" ");
        for (String type : types)
        {
            Serializable serializable = super.create(type);
            if(serializable != null)
            {
                return serializable;
            }
        }

        return null;
    }



}
