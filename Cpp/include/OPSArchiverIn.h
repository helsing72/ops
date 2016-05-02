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

#ifndef ops_OPSArchiverInH
#define ops_OPSArchiverInH

#include "ByteBuffer.h"
#include "SerializableInheritingTypeFactory.h"
#include "Serializable.h"
#include "ArchiverInOut.h"
#include <vector>

namespace ops
{

    class OPSArchiverIn : public ArchiverInOut
    {
    public:

        OPSArchiverIn(ByteBuffer* _buf, SerializableInheritingTypeFactory* factory)
        {
            buf = _buf;
            this->factory = factory;
        }

        ~OPSArchiverIn()
        {
        }

        void inout(const std::string& name, bool& value)
        {
            UNUSED(name)
            value = buf->ReadChar() > 0;
        }

        void inout(const std::string& name, char& value)
        {
            UNUSED(name)
            value = buf->ReadChar();
        }

        void inout(const std::string& name, int& value)
        {
            UNUSED(name)
            value = buf->ReadInt();
        }

        void inout(const std::string& name, __int16& value)
        {
            UNUSED(name)
            value = buf->ReadShort();
        }

        void inout(const std::string& name, __int64& value)
        {
            UNUSED(name)
            value = buf->ReadLong();
        }

        void inout(const std::string& name, float& value)
        {
            UNUSED(name)
            value = buf->ReadFloat();
        }

        void inout(const std::string& name, double& value)
        {
            UNUSED(name)
            value = buf->ReadDouble();
        }

        void inout(const std::string& name, std::string& value)
        {
            UNUSED(name)
            value = buf->ReadString();
        }

		void inout(const std::string& name, char* buffer, int bufferSize)
		{
            UNUSED(name)
			buf->ReadChars(buffer, bufferSize);
		}

		void inout(const std::string& name, Serializable& value)
        {
            UNUSED(name)
            std::string types = buf->ReadString();
            value.serialize(this);
        }

        Serializable* inout(const std::string& name, Serializable* value, int element)
        {
            UNUSED(name)
            UNUSED(value)
            UNUSED(element)
            std::string types = buf->ReadString();
            Serializable* newSer = factory->create(types);
            if (newSer != NULL)
            {
                newSer->serialize(this);
            }

            return newSer;
        }

        Serializable* inout(const std::string& name, Serializable* value)
        {
            UNUSED(name)
            if (value != NULL)//Either we do this or we initiialize object to NULL in generated code.
            {
                delete value;
            }
            std::string types = buf->ReadString();
            Serializable* newSer = factory->create(types);
            if (newSer != NULL)
            {
                //Do this to preserve type information even if slicing has occured.
                ((OPSObject*) newSer)->typesString = types;

                newSer->serialize(this);
            }

            return newSer;
        }

        void inout(const std::string& name, std::vector<bool>& value)
        {
            UNUSED(name)
            buf->ReadBooleans(value);
        }

        void inout(const std::string& name, std::vector<char>& value)
        {
            UNUSED(name)
            buf->ReadBytes(value);
        }

        void inout(const std::string& name, std::vector<int>& value)
        {
            UNUSED(name)
            buf->ReadInts(value);
        }

        void inout(const std::string& name, std::vector<__int16>& value)
        {
            UNUSED(name)
            buf->ReadShorts(value);
        }

        void inout(const std::string& name, std::vector<__int64>& value)
        {
            UNUSED(name)
            buf->ReadLongs(value);
        }

        void inout(const std::string& name, std::vector<float>& value)
        {
            UNUSED(name)
            buf->ReadFloats(value);
        }

        void inout(const std::string& name, std::vector<double>& value)
        {
            UNUSED(name)
            buf->ReadDoubles(value);
        }

        void inout(const std::string& name, std::vector<std::string>& value)
        {
            UNUSED(name)
            buf->ReadStrings(value);
        }

        int beginList(const std::string& name, int size)
        {
            UNUSED(name)
            UNUSED(size)
            return buf->ReadInt();
        }

        void endList(const std::string& name)
        {
            UNUSED(name)
            //Nothing to do in this implementation
        }

        /*template <class SerializableTypeVector> SerializableTypeVector archiveSerializables(ArchiverInOut* archive, SerializableTypeVector vec)
        {
                int size = archive->inout("size", size);
                for(unsigned int i = 0; i < size; i++)
                {
                        vec[i] = archive->inout((Serializable*)NULL);
                }
                return vec;

        }*/


    private:
        ByteBuffer* buf;
        SerializableInheritingTypeFactory* factory;
    };


}
#endif
