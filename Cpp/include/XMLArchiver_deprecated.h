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

#ifndef XMLArchiverH
#define XMLArchiverH

#include <string>

namespace ops 
{
	template <class Type>
	class nvp
	{
	public:
		std::string name;
		Type value;
		nvp(std::string name_, Type v) : name(name_), value(v)
		{
			
		}		
	};
	template <class Serializable>
	class nop
	{
	public:
		std::string name;
		Serializable value;
		nop(std::string name_, Serializable v) : name(name_), value(v)
		{		
		}
		template <class Archiver>
		void serialize(Archiver& archive)
		{
			archive << std::cout << "<" << n.name << ">"; 
			value.serialize(archive);			
		}
	};
	class XMLArchiver 
	{
	public:
		void operator<<(int v)
		{
			std::cout << v;
		}
		void operator<<(char v)
		{
			std::cout << v;
		}
		void operator<<(__int16 v)
		{
			std::cout  << v;
		}
		void operator<<(__int64 v)
		{
			std::cout << v;
		}
		void operator<<(float v)
		{
			std::cout << v;
		}
		void operator<<(double v)
		{
			std::cout << v;
		}
				
		
		void operator<<(nvp& n)
		{
			std::cout << "<" << n.name << ">" << n.value << "</" << n.name << ">\n";		
		}
		template <class Nop>
		void operator<<(Nop& n)
		{
			std::cout << "<" << n.name << ">";
			n.serialize(this);
		}

	};


}
#endif
#endif
