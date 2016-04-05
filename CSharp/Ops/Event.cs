///////////////////////////////////////////////////////////
//  Event.cs
//  Implementation of the Class Event
//  Created on:      12-nov-2011 09:25:30
//  Author:
///////////////////////////////////////////////////////////

using System;

namespace Ops 
{
	public class Event : Observable 
    {
        public void FireEvent(object args)
        {
            NotifyObservers(args);
        }
        public void FireEvent()
        {
            NotifyObservers(null);
        }

	}

}