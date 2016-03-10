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
#ifndef ops_McUdpSendDataHandler_h
#define	ops_McUdpSendDataHandler_h


//#include "Participant.h"
#include "SendDataHandler.h"
#include <map>
#include "Sender.h"
#include "OPSObject.h"
#include "Lockable.h"
#include "MemoryMap.h"
#include "TimeHelper.h"
#include <iostream>
#include <sstream>
#include <stack>

namespace ops
{

    class McUdpSendDataHandler : public SendDataHandler
    {
    public:

        McUdpSendDataHandler(/*Participant* part*/ std::string localInterface, int ttl, __int64 outSocketBufferSize = 16000000)
        {
            //this->participant = part;
            sender = Sender::createUDPSender(localInterface, ttl, outSocketBufferSize);
        }

        bool sendData(char* buf, int bufSize, Topic& topic)
        {
            SafeLock lock(&mutex);

            std::map<std::string, IpPortPair> topicSinks = topicSinkMap[topic.getName()];
            std::map<std::string, IpPortPair>::iterator it;

            bool result = true;
            //Loop over all sinks and send data, remove items that isn't "alive".

            std::stack<std::string> sinksToDelete;

            for (it = topicSinks.begin(); it != topicSinks.end(); it++)
            {
                //Check if this sink is alive
                if (it->second.isAlive())
                {
                    result &= sender->sendTo(buf, bufSize, it->second.ip, it->second.port);
                }
                else //Remove it.
                {
					//std::cout << topic.getName() << " removing " << it->second.getKey() << std::endl;
                    sinksToDelete.push(it->second.getKey());
                }
            }
            while (!sinksToDelete.empty())
            {
                topicSinkMap[topic.getName()].erase(topicSinkMap[topic.getName()].find(sinksToDelete.top()));
                sinksToDelete.pop();
            }

            return result;
        }

        void addSink(std::string& topic, std::string& ip, int& port)
        {

            SafeLock lock(&mutex);
            //TODO: decide what class will handle serialization.
            //First, check if the MemoryMap we have allocated is big enough to deal with this topic.
            //int nrSegs = topic.getSampleMaxSize() / OPSConstants::PACKET_MAX_SIZE + 1;
            //if(memMap->getNrOfSegments() < nrSegs)
            //{
            //	//We need a bigger memMap to take care of serialization for data sent on this topic.
            //	SafeLock lock(&mutex);
            //	delete memMap;
            //	memMap = new MemoryMap(nrSegs, OPSConstants::PACKET_MAX_SIZE);
            //}

            //Secondly, check if we already have any sink for this topic, if not add a new sink map for this topic.
            IpPortPair ipPort(ip, port);
            if (topicSinkMap.find(topic) == topicSinkMap.end())
            {
                //We have no sinks for this topic. Lets add a new sink map
                std::map<std::string, IpPortPair> newIpPortMap;

                //And add the new sink to the map.
                newIpPortMap[ipPort.getKey()] = ipPort;


                topicSinkMap[topic] = newIpPortMap;

                //std::cout << topic << " added as new sink " << ipPort.getKey() << std::endl;

                return;
            }
            else
            {

                if (topicSinkMap[topic].find(ipPort.getKey()) == topicSinkMap[topic].end())
                {

                    //We already have a map of sinks for this topic lets just add the new sink to the map.
                    topicSinkMap[topic][ipPort.getKey()] = ipPort;
                    //std::cout << topic << " added to existing sink " << ipPort.getKey() << std::endl;

                    return;
                }
                else //this sink is already registred with this topic
                {
                    topicSinkMap[topic][ipPort.getKey()].feedWatchdog();
                    return;
                }

            }
        }

        virtual ~McUdpSendDataHandler()
        {
            SafeLock lock(&mutex);
            delete sender;
			sender = NULL;
        }

    private:
        //Participant* participant;
        //std::map<std::string, Sender*> senders;
        //moved to base class Sender* sender;

        // MemoryMap* memMap;

        //moved to base class Lockable mutex;

        class IpPortPair
        {
        public:

            IpPortPair(std::string ip, int port)
            {
                this->ip = ip;
                this->port = port;
                this->lastTimeAlive = TimeHelper::currentTimeMillis();
            }

            IpPortPair()
            {

            }

            bool isAlive()
            {
                return (TimeHelper::currentTimeMillis() - lastTimeAlive) < ALIVE_TIMEOUT;
            }

            void feedWatchdog()
            {
                lastTimeAlive = TimeHelper::currentTimeMillis();
            }

            std::string getKey()
            {
                std::stringstream ss;
                ss << ip << ":" << port;
                return ss.str();
            }
            std::string ip;
            int port;
            __int64 lastTimeAlive;
            const static int ALIVE_TIMEOUT = 3000;
        };

        std::map<std::string, std::map<std::string, IpPortPair> > topicSinkMap;


    };


}

#endif
