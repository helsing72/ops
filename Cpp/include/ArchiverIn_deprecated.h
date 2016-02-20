#ifdef NOT_USED_ANYMORE
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

#ifndef ArchiverInH
#define ArchiverInH

#include "Deserializable.h"

namespace ops
{
//Pure virtual interface.
class ArchiverIn
{
public:
	virtual char getByte(std::string name) = 0;
    virtual int getInt(std::string name) = 0;
    virtual __int16 getShort(std::string name) = 0;
    virtual __int64 getLong(std::string name) = 0;
    virtual float getFloat(std::string name) = 0;
    virtual double getDouble(std::string name) = 0;
    virtual std::string getString(std::string name) = 0;
    virtual void getDeserializable(std::string name, Deserializable* deserializable) = 0;
    virtual int getNrElements(std::string name) = 0;
    virtual void getElement(std::string name, int i, Deserializable* d) = 0;   
};
}

#endif
#endif