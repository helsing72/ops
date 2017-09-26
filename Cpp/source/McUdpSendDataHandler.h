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

#include <map>
#include <iostream>
#include <sstream>
#include <stack>

#include "SendDataHandler.h"
#include "Sender.h"
#include "OPSObject.h"
#include "Lockable.h"
#include "MemoryMap.h"
#include "TimeHelper.h"

namespace ops
{

    class McUdpSendDataHandler : public SendDataHandler
    {
    public:
        McUdpSendDataHandler(IOService* ioService, Address_T localInterface, int ttl, __int64 outSocketBufferSize = 16000000)
        {
            sender = Sender::createUDPSender(ioService, localInterface, ttl, outSocketBufferSize);
        }

        bool sendData(char* buf, int bufSize, Topic& topic)
        {
            SafeLock lock(&mutex);

            Entry_T& topicSinks = topicSinkMap[topic.getName()];
            std::map<InternalKey_T, IpPortPair>::iterator it;

            bool result = true;

            std::stack<InternalKey_T> sinksToDelete;

            //Loop over all sinks and send data, remove items that isn't "alive".
            for (it = topicSinks.portMap.begin(); it != topicSinks.portMap.end(); ++it)
            {
                //Check if this sink is alive
                if (it->second.isAlive()) {
                    //std::cout << topic.getName() << ", sending to sink: " << it->second.getKey() << std::endl;
                    result &= sender->sendTo(buf, bufSize, it->second._ip, it->second._port);
                }
                else //Remove it.
                {
                    //std::cout << topic.getName() << ", removing sink: " << it->second.getKey() << std::endl;
                    sinksToDelete.push(it->second.getKey());
                }
            }
            while (!sinksToDelete.empty()) {
                topicSinks.portMap.erase(topicSinks.portMap.find(sinksToDelete.top()));
                sinksToDelete.pop();
            }
            return result;
        }

		void addSink(std::string& topic, std::string& ip, int& port, bool staticRoute = false)
		{
            SafeLock lock(&mutex);

			IpPortPair ipPort(ip, port, staticRoute);

			//Check if we already have any sinks for this topic
            if (topicSinkMap.find(topic) == topicSinkMap.end())
            {
                //We have no sinks for this topic.
                Entry_T ent;
                ent.staticRoute = staticRoute;

                //Add the new sink to the port map.
                ent.portMap[ipPort.getKey()] = ipPort;

                //And add the port map to the sink map
                topicSinkMap[topic] = ent;

                //std::cout << topic << ", added first sink: " << ipPort.getKey() << std::endl;
            }
            else
            {
				Entry_T& topicSinks = topicSinkMap[topic];

				//If created as static route, we only add sinks that are static
				if ( (!topicSinks.staticRoute) || (topicSinks.staticRoute && staticRoute)) {
					//Check if sink already exist
					if (topicSinks.portMap.find(ipPort.getKey()) == topicSinks.portMap.end())
					{
						//No, add the new sink to the map.
						topicSinks.portMap[ipPort.getKey()] = ipPort;

						//std::cout << topic << ", added next sink: " << ipPort.getKey() << std::endl;
					}
					else
					{
						//this sink is already registered with this topic, just update
						topicSinks.portMap[ipPort.getKey()].feedWatchdog(staticRoute);
						//std::cout << topic << ", feedWatchDog for sink: " << ipPort.getKey() << std::endl;
					}
				}
            }
        }

        virtual ~McUdpSendDataHandler()
        {
            SafeLock lock(&mutex);
            delete sender;
			sender = nullptr;
        }

    private:
        class IpPortPair
        {
        public:
            IpPortPair(std::string ip, int port, bool alwaysAlive):
				_ip(ip), _port(port), _alwaysAlive(alwaysAlive)
            {
                _lastTimeAlive = TimeHelper::currentTimeMillis();
            }

            IpPortPair():
				_port(0), _alwaysAlive(false), _lastTimeAlive(0)
            {
            }

            bool isAlive()
            {
                return _alwaysAlive || ((TimeHelper::currentTimeMillis() - _lastTimeAlive) < ALIVE_TIMEOUT);
            }

            void feedWatchdog(bool alwaysAlive)
            {
                _alwaysAlive |= alwaysAlive;	// once set to true, always true
                _lastTimeAlive = TimeHelper::currentTimeMillis();
            }

			InternalKey_T getKey()
            {
				InternalKey_T key(_ip.c_str());
				key += ':';
				key += NumberToString(_port);
                return key;
            }

			Address_T _ip;
            int _port;
			bool _alwaysAlive;
			__int64 _lastTimeAlive;
            const static int ALIVE_TIMEOUT = 3000;
        };

		typedef struct {
			bool staticRoute;
			std::map<InternalKey_T, IpPortPair> portMap;
		} Entry_T;

		std::map<ObjectName_T, Entry_T > topicSinkMap;
    };
}
#endif
