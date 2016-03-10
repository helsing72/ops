///////////////////////////////////////////////////////////
//  Reply.cs
//  Implementation of the Class Reply
//  Created on:      12-nov-2011 09:25:35
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public class Reply : OPSObject 
    {
		public string message = "";
		public bool requestAccepted = false;
		public string requestId = "";

        public override void Serialize(IArchiverInOut archive)
        {
            base.Serialize(archive);
            requestId = archive.Inout("requestId", requestId);
            requestAccepted = archive.Inout("requestAccepted", requestAccepted);
            message = archive.Inout("message", message);
		}

	}

}