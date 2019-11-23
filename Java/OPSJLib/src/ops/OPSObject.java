/**
*
* Copyright (C) 2006-2009 Anton Gravestam.
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
import configlib.Serializable;
import java.io.IOException;
import java.util.Arrays;
import java.util.Vector;

/**
 *
 * @author Anton Gravestam
 */
public class OPSObject implements Serializable
{
    protected String key = "";
    protected String typesString = "";
    public byte[] spareBytes = new byte[0];


    public OPSObject()
    {

    }

    protected void appendType(String type)
    {
        typesString = type + " " + typesString;
    }

    public void setKey(String key)
    {
        this.key = key;
    }

    public String getKey()
    {
        return key;
    }

    public String getTypesString()
    {
        return typesString;
    }

    @Override
    public Object clone()
    {
        OPSObject cloneResult = new OPSObject();
        fillClone(cloneResult);
        return cloneResult;
    }

    public void fillClone(OPSObject cloneResult)
    {
        cloneResult.typesString = this.typesString;
        cloneResult.key = this.key;
        cloneResult.spareBytes = Arrays.copyOf(this.spareBytes, this.spareBytes.length);
    }

    public void serialize(ArchiverInOut archive) throws IOException
    {
        key = archive.inout("key", key);
    }

    @Override
    public String toString()
    {
        return getClass().getName();
    }









}
