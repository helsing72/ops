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

#ifndef ops_ArchiverInOutH
#define ops_ArchiverInOutH

#include "OPSTypeDefs.h"
#include "Serializable.h"
#include <vector>
#include <string>

namespace ops
{
    //Pure virtual interface.

    class ArchiverInOut
    {
    public:

        virtual ~ArchiverInOut()
        {
        }

        virtual void inout(const std::string& name, bool& value) = 0;
        virtual void inout(const std::string& name, char& value) = 0;
        virtual void inout(const std::string& name, int& value) = 0;
        virtual void inout(const std::string& name, __int16& value) = 0;
        virtual void inout(const std::string& name, __int64& value) = 0;
        virtual void inout(const std::string& name, float& value) = 0;
        virtual void inout(const std::string& name, double& value) = 0;
        virtual void inout(const std::string& name, std::string& value) = 0;
        virtual void inout(const std::string& name, Serializable& value) = 0;

///LA
		virtual void inout(const std::string& name, char* buffer, int bufferSize) = 0;
///LA

		virtual Serializable* inout(const std::string& name, Serializable* value) = 0;

        virtual Serializable* inout(const std::string& name, Serializable* value, int element) = 0;

        virtual void inout(const std::string& name, std::vector<bool>& value) = 0;
        virtual void inout(const std::string& name, std::vector<char>& value) = 0;
        virtual void inout(const std::string& name, std::vector<int>& value) = 0;
        virtual void inout(const std::string& name, std::vector<__int16>& value) = 0;
        virtual void inout(const std::string& name, std::vector<__int64>& value) = 0;
        virtual void inout(const std::string& name, std::vector<float>& value) = 0;
        virtual void inout(const std::string& name, std::vector<double>& value) = 0;
        virtual void inout(const std::string& name, std::vector<std::string>& value) = 0;

        template <class SerializableType> void inout(const std::string& name, std::vector<SerializableType>& vec, SerializableType prototype)
        {
            int size = beginList(name, vec.size());
            if ((int) vec.size() < size)
            {
                vec.clear();
                vec.reserve(size);
                vec.resize(size, prototype);
                for (int i = 0; i < size; i++)
                {
                    inout(std::string("element"), vec[i]);
                }

                //vec.resize(size, new SerializableType());
            }
            else
            {
                for (int i = 0; i < size; i++)
                {
                    inout(std::string("element"), vec[i]);
                }
            }
            endList(name);

        }

        template <class SerializableType> void inout(const std::string& name, std::vector<SerializableType*>& vec)
        {
            int size = beginList(name, vec.size());
            if ((int) vec.size() < size)
            {
                vec.clear();
                vec.reserve(size);
                for (int i = 0; i < size; i++)
                {
                    vec.push_back((SerializableType*) inout(std::string(name), (Serializable*) NULL, i));
                }

                //vec.resize(size, new SerializableType());
            }
            else
            {
                for (int i = 0; i < size; i++)
                {
                    vec[i] = (SerializableType*) inout(std::string(name), vec[i], i);
                }
            }
            endList(name);
        }

        /*template <class SerializableTypeVector> void in(std::string name&, SerializableTypeVector& vec)
        {
                int size = beginList(name, size);
                vec.clear();
                for(unsigned int i = 0; i < size; i++)
                {
                        vec.push_back(*inout((Serializable*)NULL));
                }
        }*/
    protected:
        virtual int beginList(const std::string& name, int i) = 0;
        virtual void endList(const std::string& name) = 0;

    };
}

#endif
