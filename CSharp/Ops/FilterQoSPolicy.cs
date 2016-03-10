///////////////////////////////////////////////////////////
//  FilterQoSPolicy.cs
//  Implementation of the Interface FilterQoSPolicy
//  Created on:      12-nov-2011 09:25:30
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public interface IFilterQoSPolicy 
    {
		/// 
		/// <param name="o"></param>
		bool ApplyFilter(OPSObject o);
	}

}