///////////////////////////////////////////////////////////
//  Transport.cs
//  Implementation of the Class Channel
//  Created on:      18-oct-2016
//  Author:
///////////////////////////////////////////////////////////

using System.Collections.Generic;

namespace Ops 
{
	public class Transport : OPSObject 
    {
        public string channelID = "";
        public List<string> topics = new List<string>();

        public Transport()
        {
            AppendType("Transport");
        }

        public override void Serialize(IArchiverInOut archive)
        {
            // NOTE. Keep this in sync with the C++ version, so it in theory is possible to send these as objects.
            // We need to serialize fields in the same order as C++.
            //OPSObject::serialize(archiver);
            base.Serialize(archive);
            
            //archiver->inout(std::string("channelID"), channelID);
            channelID = archive.Inout("channelID", channelID);

            //archiver->inout(std::string("topics"), topics);
            topics = (List<string>)archive.InoutStringList("topics", topics);
        }
	}
}
