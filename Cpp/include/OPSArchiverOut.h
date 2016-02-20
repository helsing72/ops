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

#ifndef ops_OPSArchiverOutH
#define ops_OPSArchiverOutH

#include "ByteBuffer.h"
#include "Serializable.h"
#include "ArchiverInOut.h"
#include "OPSTypeDefs.h"
#include <vector>

namespace ops
{

    class OPSArchiverOut : public ArchiverInOut
    {
    public:

        OPSArchiverOut(ByteBuffer* _buf)
        {
            buf = _buf;
        }

        ~OPSArchiverOut()
        {

        }

        void inout(const std::string& name, bool& value)
        {
            UNUSED(name)
            char ch = 0;
            value ? ch = 1 : ch = 0;
            buf->WriteChar(ch);
        }

        void inout(const std::string& name, char& value)
        {
            UNUSED(name)
            buf->WriteChar(value);
        }

        void inout(const std::string& name, int& value)
        {
            UNUSED(name)
            buf->WriteInt(value);
        }

        void inout(const std::string& name, __int16& value)
        {
            UNUSED(name)
            buf->WriteShort(value);
        }

        void inout(const std::string& name, __int64& value)
        {
            UNUSED(name)
            buf->WriteLong(value);
        }

        void inout(const std::string& name, float& value)
        {
            UNUSED(name)
            buf->WriteFloat(value);
        }

        void inout(const std::string& name, double& value)
        {
            UNUSED(name)
            buf->WriteDouble(value);
        }

        void inout(const std::string& name, std::string& value)
        {
            UNUSED(name)
            buf->WriteString(value);
        }

///LA
		void inout(const std::string& name, char* buffer, int bufferSize)
		{
            UNUSED(name)
			buf->WriteChars(buffer, bufferSize);
		}
///LA

        void inout(const std::string& name, Serializable& value)
        {
            UNUSED(name)
            std::string typeS = ((OPSObject&) value).getTypeString();
            buf->WriteString(typeS);
            value.serialize(this);
        }

        Serializable* inout(const std::string& name, Serializable* value, int element)
        {
            UNUSED(name)
            UNUSED(value)
            UNUSED(element)
            std::string typeS = ((OPSObject*) value)->getTypeString();
            buf->WriteString(typeS);
            value->serialize(this);
            return value;
        }

        Serializable* inout(const std::string& name, Serializable* value)
        {
            UNUSED(name)
            std::string typeS = ((OPSObject*) value)->getTypeString();
            buf->WriteString(typeS);
            value->serialize(this);
            return value;
        }

        void inout(const std::string& name, std::vector<bool>& value)
        {
            UNUSED(name)
            buf->WriteBooleans(value);
        }

        void inout(const std::string& name, std::vector<char>& value)
        {
            UNUSED(name)
            buf->WriteBytes(value);
        }

        void inout(const std::string& name, std::vector<int>& value)
        {
            UNUSED(name)
            UNUSED(value)
            buf->WriteInts(value);
        }

        void inout(const std::string& name, std::vector<__int16>& value)
        {
            //Not implemented
            UNUSED(name)
            UNUSED(value)
        }

        void inout(const std::string& name, std::vector<__int64>& value)
        {
            UNUSED(name)
            buf->WriteLongs(value);
        }

        void inout(const std::string& name, std::vector<float>& value)
        {
            UNUSED(name)
            buf->WriteFloats(value);
        }

        void inout(const std::string& name, std::vector<double>& value)
        {
            UNUSED(name)
            buf->WriteDoubles(value);
        }

        void inout(const std::string& name, std::vector<std::string>& value)
        {
            UNUSED(name)
            buf->WriteStrings(value);
        }

        int beginList(const std::string& name, int size)
        {
            UNUSED(name)
            buf->WriteInt(size);
            return size;
        }

        void endList(const std::string& name)
        {
            //Nothing to do in this implementation
            UNUSED(name)
        }
    private:
        ByteBuffer* buf;

    };
}

#endif
