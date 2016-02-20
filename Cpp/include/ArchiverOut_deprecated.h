#ifdef NOT_USED_ANYMORE
/**
* 
* Copyright (C) 2006-2009 Anton Gravestam.
*
* This notice apply to all source files, *.cpp, *.h, *.java, and *.cs in this directory 
* and all its subdirectories if nothing else is explicitly stated within the source file itself.
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

#ifndef ArchiverOutH
#define ArchiverOutH

#include "Serializable.h"

namespace ops
{
//Pure virtual interface.
class ArchiverOut
{
public:
	virtual void put(std::string& name, Serializable* v) = 0;
    virtual void put(std::string& name, std::string v) = 0;
    virtual void put(std::string& name, char v) = 0;
    virtual void put(std::string& name, double v) = 0;
    virtual void put(std::string& name, float v) = 0;
    virtual void put(std::string& name, int v) = 0;
    virtual void put(std::string& name, long v) = 0;
    virtual void put(std::string& name, __int16 v) = 0;
	virtual void putStartList(std::string& name, int size) = 0;
	virtual void putElement(std::string& listName, Serializable* v) = 0;
	virtual void putEndList(std::string& listName) = 0;
	//template <class ElementType>
	//virtual void put(std::string name, std::vector<ElementType> collection) = 0;
};

}
#endif
#endif