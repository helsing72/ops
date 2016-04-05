///////////////////////////////////////////////////////////
//  Notifier.cs
//  Implementation of the Class Notifier
//  Created on:      12-nov-2011 09:25:32
//  Author:
///////////////////////////////////////////////////////////

using System.Collections.Generic;
using System.Runtime.CompilerServices;  // Needed for the "MethodImpl" synchronization attribute

namespace Ops 
{
	public class Notifier<T> 
    {
		internal List<IListener<T>> listeners = new List<IListener<T>>();

        public void NotifyListeners(T arg)
        {
            foreach (IListener<T> listener in listeners)
            {
                listener.OnNewEvent(this, arg);
            }
        }

        public void RemoveListener(IListener<T> listener)
        {
            listeners.Remove(listener);
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public void AddListener(IListener<T> listener)
        {
            listeners.Add(listener);
        }

	}

}