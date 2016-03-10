///////////////////////////////////////////////////////////
//  Listener.cs
//  Implementation of the Interface Listener
//  Created on:      12-nov-2011 09:25:31
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public interface IListener<T>
    {
		void OnNewEvent(Notifier<T> notifier, T arg);
	}

}