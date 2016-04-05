///////////////////////////////////////////////////////////
//  InProcessTransport.cs
//  Implementation of the Class InProcessTransport
//  Created on:      12-nov-2011 09:25:30
//  Author:
///////////////////////////////////////////////////////////


using System;
using System.Collections.Generic;
using System.Runtime.CompilerServices;  // Needed for the "MethodImpl" synchronization attribute

namespace Ops 
{
	public class InProcessTransport : EasyThread 
    {
        internal BlockingQueue<OPSMessage> blockingQueue = new BlockingQueue<OPSMessage>();
        private volatile bool keepRunning;
		private List<Subscriber> subscribers = new List<Subscriber>();

        /// @param message will be out on the underlying queue
        public void PutMessage(OPSMessage message)
        {
            blockingQueue.Put(message);
        }
        
        /// @param originalMessage will be copied and put on the underlying queue.
        public void CopyAndPutMessage(OPSMessage originalMessage)
        {
            PutMessage((OPSMessage) originalMessage.Clone());
        }

        // Returns null if blocking queuue is stopped.
        public OPSMessage TakeMessage()
        {
            return blockingQueue.Take();
        }

        protected override void Run()
        {
            keepRunning = true;
            while(keepRunning)
            {
                OPSMessage newMessage = TakeMessage();
                if (newMessage == null)
                {
                    break;
                }
                newMessage.SetQosMask(1);
                NotifySubscribers(newMessage);
            }
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public void AddSubscriber(Subscriber subscriber)
        {
            subscribers.Add(subscriber);
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public void RemoveSubscriber(Subscriber subscriber)
        {
            subscribers.Remove(subscriber);
        }

        public void StopTransport()
        {
            blockingQueue.Stop();
            keepRunning = false;
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        private void NotifySubscribers(OPSMessage newMessage)
        {
            foreach (Subscriber subscriber in subscribers)
            {
                if(subscriber.GetTopic().GetName().Equals(newMessage.GetTopicName()))
                {
                    subscriber.NotifyNewOPSMessage(newMessage);
                }
            }
        }

	}

}