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
#include "SerializableInheritingTypeFactory.h"

#ifndef REPLACE_TRANSPORT_LAYER
#include <boost/algorithm/string/split.hpp> 
#include <boost/algorithm/string/classification.hpp>
#endif

namespace ops
{

#ifdef REPLACE_TRANSPORT_LAYER
	void split(const std::string& s, char c, std::vector<std::string>& v)
	{
		std::string::size_type i = 0;
		std::string::size_type j = s.find(c);

		while (j != std::string::npos) {
			v.push_back(s.substr(i, j - i));
			i = ++j;
			j = s.find(c, j);
		}
		v.push_back(s.substr(i, s.length()));
	}
#endif

	/**
     * Tries to construct the most specialized object in the given typeString list
 	 */
	Serializable* SerializableInheritingTypeFactory::create(std::string& typeString)
	{
		std::vector<std::string> types;
#ifdef REPLACE_TRANSPORT_LAYER
		split(typeString, ' ', types);
#else
		boost::algorithm::split(types, typeString, boost::algorithm::is_any_of(" "));
#endif

		for (unsigned int i = 0; i < types.size(); i++) {
			Serializable* serializable = SerializableCompositeFactory::create(types[i]);
			if (serializable != NULL) {
				return serializable;
			}
		}
		return NULL;
	}

}
