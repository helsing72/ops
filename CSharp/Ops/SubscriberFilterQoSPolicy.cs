///////////////////////////////////////////////////////////
//  SubscriberFilterQoSPolicy.cs
//  Implementation of the Interface SubscriberFilterQoSPolicy
//  Created on:      12-nov-2011 09:25:36
//  Author: SubscriberFilterQoSPolicies may be used to prevent data samples to be delivered to the application layer.
///////////////////////////////////////////////////////////

namespace Ops 
{
	public interface SubscriberFilterQoSPolicy 
    {
		// Should return true if the sample "passes the filter" and false if not.
		// Returning false will prevent the data to be made available to the application.
		bool ApplyFilter(OPSObject data);
	}

}