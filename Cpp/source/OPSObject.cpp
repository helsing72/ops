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
#include "OPSTypeDefs.h"
#include "OPSObject.h"

namespace ops
{
	OPSObject::OPSObject() : Serializable()
    {
        key = "k";
		typesString = "";
        
    }
	OPSObject* OPSObject::clone()
	{
		OPSObject* obj = new OPSObject();
		fillClone(obj);
		return obj;
	}
	void OPSObject::fillClone(OPSObject* obj)const
	{
		obj->key = key;
		obj->typesString = typesString;
	}
    std::string OPSObject::getKey()
    {
         return key;
    }
	const std::string& OPSObject::getTypeString()
	{
		return typesString;
	}
	void OPSObject::setKey(std::string k)
	{
		key = k;
	}

	void OPSObject::serialize(ArchiverInOut* archive)
	{
		archive->inout(std::string("key"), key);
	}
	
    OPSObject::~OPSObject()
    {
    }
}
