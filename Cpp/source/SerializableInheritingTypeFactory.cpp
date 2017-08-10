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

namespace ops
{

	void split(const TypeId_T& s, char c, std::vector<TypeId_T>& v)
	{
		TypeId_T::size_type i = 0;
		TypeId_T::size_type j = s.find(c);

		while (j != TypeId_T::npos) {
			v.push_back(s.substr(i, j - i));
			i = ++j;
			j = s.find(c, j);
		}
		v.push_back(s.substr(i, s.length()));
	}

	/**
     * Tries to construct the most specialized object in the given typeString list
 	 */
	Serializable* SerializableInheritingTypeFactory::create(TypeId_T& typeString)
	{
		std::vector<TypeId_T> types;
		split(typeString, ' ', types);

		for (unsigned int i = 0; i < types.size(); i++) {
			Serializable* serializable = SerializableCompositeFactory::create(types[i]);
			if (serializable != NULL) {
				return serializable;
			}
		}
		return NULL;
	}

}
