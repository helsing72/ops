///////////////////////////////////////////////////////////
//  MessageFilter.cs
//  Implementation of the Interface MessageFilter
//  Created on:      12-nov-2011 09:25:31
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public interface IMessageFilter 
    {
		/// 
		/// <param name="o"></param>
		bool ApplyFilter(OPSMessage o);
	}

}