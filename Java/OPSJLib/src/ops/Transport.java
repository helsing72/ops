/**
 *
 * Copyright (C) 2016 Lennart Andersson.
 *
 * This file is part of OPS (Open Publish Subscribe).
 *
 * OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
 */
package ops;

import configlib.ArchiverInOut;
import java.io.IOException;

public class Transport extends OPSObject
{
    public String channelID = "";
		public java.util.Vector<String> topics = new java.util.Vector<String>();

    public Transport()
    {
        appendType("Transport");
    }

		@Override
    public void serialize(ArchiverInOut archive) throws IOException
    {
        // NOTE. Keep this in sync with the C++ version, so it in theory is possible to send these as objects.
        // We need to serialize fields in the same order as C++.
        //OPSObject::serialize(archiver);
        super.serialize(archive);

        //archiver->inout(std::string("channelID"), channelID);
        channelID = archive.inout("channelID", channelID);

        //archiver->inout(std::string("topics"), topics);
				topics = (java.util.Vector<String>) archive.inoutStringList("topics", topics);
    }
}
