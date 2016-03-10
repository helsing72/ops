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

#ifndef ops_OPSObject_h
#define ops_OPSObject_h

#include <string>
#include "Serializable.h"
#include "ArchiverInOut.h"


namespace ops
{
    
    ///Base class for object that can be serialized with OPSArchivers
	///LA Moved inheritance of Reservable to OPSMessage
	class OPSObject :  public Serializable
    {
        
        friend class ByteBuffer;
        //friend class OPSObjectHelper;
        friend class OPSArchiverIn;
        
    protected:
        //Should only be set by the Publisher at publication time and by ByteBuffer at deserialization time.
        std::string key;
        std::string typesString;

		void appendType(const std::string& type)
		{
			typesString = type + " " + typesString;
		}
    public:
        std::string getKey();
		const std::string& getTypeString();
		void setKey(std::string k);
		virtual void serialize(ArchiverInOut* archive);

		///Bytes that hold unserialized data for this object.
		///This happens if a type can not be fully understood by a participants type support.
		std::vector<char> spareBytes;
		
		///Returns a newely allocated deep copy/clone of this object.
		virtual OPSObject* clone();

		///Fills the parameter obj with all values from this object.
		virtual void fillClone(OPSObject* obj) const;
        
    public:
		//static void deleteObjectVector(OPSObject* start);
        OPSObject();
        virtual ~OPSObject();


        
    };
}

#endif
