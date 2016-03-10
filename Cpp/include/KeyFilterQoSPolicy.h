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

//
// File:   KeyFilterQoSPolicy.h
// Author: Anton Gravestam
//
// Created on den 28 november 2007, 19:59
//

#ifndef _KEYFILTERQOSPOLICY_H
#define	_KEYFILTERQOSPOLICY_H

#include "FilterQoSPolicy.h"
#include "Lockable.h"
#include <string>
#include <vector>


namespace ops
{
    //Forward declaration//////
    class OPSObject;///////////
    ///////////////////////////
    
    class KeyFilterQoSPolicy
		: public FilterQoSPolicy, public Lockable
	{
        
    public:
		KeyFilterQoSPolicy(std::string keyString);
		KeyFilterQoSPolicy(std::vector<std::string> keyStrings);
		void setKeys(std::vector<std::string> keyStrings);
		void setKey(std::string key);
		std::vector<std::string> getKeys();

		virtual ~KeyFilterQoSPolicy();


        //Overides applyFilterQoSPolicy(OPSObject* o) in FilterQoSPolicy
        bool applyFilter(OPSObject* o);
	private:
		std::vector<std::string> keyStrings;
		std::string keyString;
        
    };
    
}

#endif	/* _KEYFILTERQOSPOLICY_H */

