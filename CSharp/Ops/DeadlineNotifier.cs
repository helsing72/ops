///////////////////////////////////////////////////////////
//  DeadlineNotifier.cs
//  Implementation of the Class DeadlineNotifier
//  Created on:      12-nov-2011 09:25:29
//  Author:
///////////////////////////////////////////////////////////

using System.Runtime.CompilerServices;  // Needed for the "MethodImpl" synchronization attribute
using System.Collections.Generic;       // Needed for the "List"
using System.Threading;                 // Needed for "Thread.Sleep"

namespace Ops 
{
	public class DeadlineNotifier : EasyThread 
    {
		private volatile bool keepRunning;
		internal List<Subscriber> subscribers = new List<Subscriber>();
		private static DeadlineNotifier theInstance;


        [MethodImpl(MethodImplOptions.Synchronized)]
        public static DeadlineNotifier GetInstance()
        {
            if (theInstance == null)
            {
                theInstance = new DeadlineNotifier();
            }
            return theInstance;
        }

        public DeadlineNotifier()
        {
            // Set the name of this thread.
            this.Name = "DeadlineNotifierThread";
        }

        /// 
        /// <param name="e"></param>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public void Add(Subscriber e)
        {
            if (subscribers.Count == 0)
            {
                theInstance.Start();
            }
            subscribers.Add(e);
        }

        /// 
        /// <param name="e"></param>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public bool Contains(Subscriber e)
        {
            return subscribers.Contains(e);
        }

        /// 
		/// <param name="o"></param>
        [MethodImpl(MethodImplOptions.Synchronized)]
        public bool Remove(Subscriber o)
        {
            bool result = subscribers.Remove(o);
            if (subscribers.Count == 0)
            {
                // We do not stop the thread here because it wouldn't be so easy to start it again.
                // Another apporach would be to signal to the thread when it shall run/pause.
                // Maybe someone in the future will rewrite this.
                //StopRunning();
            }
            return result;
		}

		protected override void Run()
        {
            keepRunning = true;
            while (keepRunning)
            {
                try 
                {
                    lock(this)
                    {
                        foreach (Subscriber subscriber in subscribers)
                        {
                            subscriber.CheckDeadline();
                        }
                    }
                    Thread.Sleep(5);
                }
                catch (ThreadStateException ex) 
                {
                    Logger.ExceptionLogger.LogException(ex);
                }                
            }
		}

		public void StopRunning(){
            keepRunning = false;
        }

	}

}