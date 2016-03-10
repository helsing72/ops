///////////////////////////////////////////////////////////
//  Request.cs
//  Implementation of the Class Request
//  Created on:      12-nov-2011 09:25:35
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public class Request : OPSObject 
    {
		public string requestId = "";

		public override void Serialize(IArchiverInOut archive)
        {
            base.Serialize(archive);
            requestId = archive.Inout("requestId", requestId);
        }

	}

}