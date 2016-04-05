///////////////////////////////////////////////////////////
//  AckData.cs
//  Implementation of the Class AckData
//  Created on:      12-nov-2011 09:25:28
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public class AckData : OPSObject 
    {
		public bool dataAccepted;
		public string id;
		public string message;

        public AckData()
        {
            dataAccepted = false;
            message = "";
            id = "";
        }

        public new object Clone() 
        { 
            //# Returns null?? Används denna klass?
            return null; 
        }
    }

}