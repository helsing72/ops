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

#ifndef ops_RequestReplyH
#define ops_RequestReplyH

#include "Topic.h"
#include "Subscriber.h"
#include "Publisher.h"
#include "KeyFilterQoSPolicy.h"
#include <sstream>


namespace ops
{

	template<class ReqType, class RepType>
    class RequestReply
    {
    public:
		RequestReply(Topic reqTopic, Topic repTopic, std::string key_) :keyFilter(key_), key(key_)	  
		{
			sub = new Subscriber(repTopic);
			sub->addFilterQoSPolicy(&keyFilter);

			pub = new Publisher(reqTopic);
			sub->start();

		}
	    RepType* request(ReqType* req, int timeout)
		{
			static int reqInt = 0;
			reqInt ++;
			req->setKey(key);
			std::stringstream ss;
			ss << key << reqInt;
			req->requestId = ss.str(); 
			
			__int64 requestLimit = TimeHelper::currentTimeMillis() + (__int64)timeout;
			sub->getDataReference();
			pub->writeOPSObject(req);
			while(TimeHelper::currentTimeMillis() < requestLimit)
			{
				if(sub->waitForNewData((int)(requestLimit - TimeHelper::currentTimeMillis())))
				{
					if(((RepType*)sub->getMessage()->getData())->requestId == req->requestId)
					{
						return (RepType*)sub->getMessage()->getData()->clone();
					}
				}
			}
			return NULL;

		}
		~RequestReply()
		{
			delete sub;
			delete pub;
		}
	private:
		Subscriber* sub;
		Publisher* pub;
		KeyFilterQoSPolicy keyFilter;
		std::string key;
        
    };
}
#endif
