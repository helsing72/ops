///////////////////////////////////////////////////////////
//  RequestReply.cs
//  Implementation of the Class RequestReply
//  Created on:      12-nov-2011 09:25:35
//  Author:
///////////////////////////////////////////////////////////

using System.Threading;

namespace Ops 
{
	public class RequestReply<ReqType, RepType> where ReqType : Request where RepType : Reply
    {
		private string key;
		internal int nrResends = 1;
		internal Publisher publisher;
		internal RepType reply;
		internal static int reqInt = 0;
		internal Subscriber subscriber;
		internal long timeout;
        internal string lastRequestId;

        public long GetTimeout()
        {
            return timeout;
        }

        public void SetTimeout(long timeout)
        {
            this.timeout = timeout;
        }

        public int GetNrResends()
        {
            return nrResends;
        }

        public void SetNrResends(int nrResends)
        {
            this.nrResends = nrResends;
        }

        public RequestReply(Topic reqTopic, Topic repTopic, string key) 
        {
            publisher = new Publisher(reqTopic);
            subscriber = new Subscriber(repTopic);
            this.key = key;
            subscriber.AddFilterQoSPolicy(new KeyFilterQoSPolicy(key));
            subscriber.Start();
        }

        public RepType Request(ReqType request)
        {
            reqInt ++;
            request.requestId = key + reqInt;
            lastRequestId = request.requestId;
            reply = null;

            Thread thread = new Thread(new ThreadStart(Run));
            thread.IsBackground = true;
            thread.Start();

            for (int i = 0; i < nrResends; i++)
            {
                publisher.WriteAsOPSObject(request);
                try
                {
                    thread.Join((int)(timeout / nrResends));
                    break;
                }
                catch (ThreadAbortException)
                {
                }
            }
     
            // Reply will be null if we don't get a reply within timeout.
            return reply;
        }

        public void Run()
        {
            reply = (RepType)subscriber.WaitForNextOpsObjectData(timeout);
            if (!reply.requestId.Equals(lastRequestId))
            {
                reply = null;
            }
        }
	}

}