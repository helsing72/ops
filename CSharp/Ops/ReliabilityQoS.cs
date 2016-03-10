///////////////////////////////////////////////////////////
//  ReliabilityQoS.cs
//  Implementation of the Class ReliabilityQoS
//  Created on:      12-nov-2011 09:25:35
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public class ReliabilityQoS : QualityOfService
    {
#pragma warning disable 414,169  // variables not used
        private int nrOfResends = 1;
        private int replyPort;
        private long resendInterval = 100;
#pragma warning restore 414,169

        public ReliabilityQoS()
        {
		}

        public string GetQoSID()
        {
            return "reliability";
        }

	}

}