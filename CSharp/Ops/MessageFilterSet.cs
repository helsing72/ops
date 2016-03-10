///////////////////////////////////////////////////////////
//  MessageFilterSet.cs
//  Implementation of the Class MessageFilterSet
//  Created on:      12-nov-2011 09:25:31
//  Author:
///////////////////////////////////////////////////////////

using System.Collections.Generic;       // Needed for the "List"
using System.Runtime.CompilerServices;  // Needed for the "MethodImpl" synchronization attribute

namespace Ops 
{
	public class MessageFilterSet : IMessageFilter 
    {
        internal List<IMessageFilter> filters = new List<IMessageFilter>();

        [MethodImpl(MethodImplOptions.Synchronized)]
        public void AddFilter(IMessageFilter filter)
        {
            filters.Add(filter);
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public void RemoveFilter(IMessageFilter filter)
        {
            filters.Remove(filter);
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public bool ApplyFilter(OPSMessage o)
        {
            foreach (IMessageFilter messageFilter in filters)
            {
                if (!messageFilter.ApplyFilter(o))
                {
                    return false;
                }
            }
            return true;
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public bool Contains(IMessageFilter messageFilter)
        {
            return filters.Contains(messageFilter);
        }

	}

}