///////////////////////////////////////////////////////////
//  DeadlineMissedFilterQoSPolicy.cs
//  Implementation of the Class DeadlineMissedFilterQoSPolicy
//  Created on:      12-nov-2011 09:25:29
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Threading;

namespace Ops 
{
	public class DeadlineMissedFilterQoSPolicy : Observable, IFilterQoSPolicy 
    {
		internal long maxSeparation;
		internal bool newUpdate = false;
		private bool stopped = false;

        // Creates a new instance of DeadlineMissedFilterQoSPolicy
        public DeadlineMissedFilterQoSPolicy(long maxSeparation)
        {
            this.maxSeparation = maxSeparation;

            Thread t = new Thread(new ThreadStart(Run));
            t.IsBackground = true;
            t.Start(); 
        }

        public bool ApplyFilter(OPSObject o)
        {
            newUpdate = true;
            return true;
        }

        public void Stop()
        {
            stopped = true;
        }

        public void Run()
        {
            try
            {
                while (!stopped)
                {
                    Thread.Sleep((int)maxSeparation);
                    if (!newUpdate)
                    {
                        NotifyObservers(null);
                    }
                    newUpdate = false;
                }
            }
            catch (ThreadAbortException ex)
            {
                Logger.ExceptionLogger.LogException(ex);
            }
        }

	}

}