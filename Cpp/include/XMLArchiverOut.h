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

#ifndef XMLArchiverOutH
#define XMLArchiverOutH

#include "ArchiverInOut.h"
#include <vector>
#include <string>
#include <iostream>
#include "OPSObject.h"


namespace ops
{
    class XMLArchiverOut : public ArchiverInOut
    {
        std::ostream& os;
        int currentTabDepth;
        const static int tabSize = 3;
        std::string topNode;
    private:

        std::string tab()
        {
            std::string ret("");
            for (int i = 0; i < currentTabDepth; i++)
                ret += ("   ");
            return ret;
        }
    public:

        XMLArchiverOut(std::ostream& os_, std::string topNode_) : os(os_), currentTabDepth(0), topNode(topNode_)
        {
            os << tab() << "<" << topNode << ">" << std::endl;
            currentTabDepth++;
        }

		// Returns true if it's an output archiver
		virtual bool isOut() { return true; }

		void close()
        {
            currentTabDepth--;
            os << tab() << "</" << topNode << ">" << std::endl;
        }

        virtual void inout(InoutName_T name, bool& value)
        {
            if (value)
            {
                os << tab() << "<" << name << ">" << "true" << "</" << name << ">\n";
            }
            else
            {
                os << tab() << "<" << name << ">" << "false" << "</" << name << ">\n";
            }
        }

        virtual void inout(InoutName_T name, char& value)
        {
            os << tab() << "<" << name << ">" << (int) value << "</" << name << ">\n";
        }

        virtual void inout(InoutName_T name, int& value)
        {
            os << tab() << "<" << name << ">" << value << "</" << name << ">\n";
        }

        virtual void inout(InoutName_T name, int16_t& value)
        {
            os << tab() << "<" << name << ">" << value << "</" << name << ">\n";
        }

        virtual void inout(InoutName_T name, int64_t& value)
        {
            os << tab() << "<" << name << ">" << value << "</" << name << ">\n";
        }

        virtual void inout(InoutName_T name, float& value)
        {
            os << tab() << "<" << name << ">" << value << "</" << name << ">\n";
        }

        virtual void inout(InoutName_T name, double& value)
        {
            os << tab() << "<" << name << ">" << value << "</" << name << ">\n";
        }

        virtual void inout(InoutName_T name, std::string& value)
        {
            os << tab() << "<" << name << ">" << value << "</" << name << ">\n";
        }

		virtual void inoutfixstring(InoutName_T name, char* value, int& size, int max_size, int idx)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(size)
			UNUSED(max_size)
			UNUSED(idx)
			///TODO
			throw ops::ArchiverException("XMLArchiverOut.inout(name, char*, int&, int) NYI");
		}

		virtual void inout(InoutName_T name, char* buffer, int bufferSize)
        {
            UNUSED(name);
            UNUSED(buffer);
            UNUSED(bufferSize);
            ///TODO
            throw ops::ArchiverException("XMLArchiverOut.inout(name, char*, int) NYI");
        }

        virtual Serializable* inout(InoutName_T name, Serializable* value, int element)
        {
            UNUSED(name);
            UNUSED(element);
            OPSObject* opsO = dynamic_cast<OPSObject*> (value);

            if (opsO != NULL)
            {
                os << tab() << "<" << "element" << " type = \"" << opsO->getTypeString() << "\" >" << "\n";
                currentTabDepth++;
                value->serialize(this);
                currentTabDepth--;
                os << tab() << "</" << "element" << ">" << "\n";
            }
            return value;

        }

        virtual Serializable* inout(InoutName_T name, Serializable* value)
        {
            OPSObject* opsO = dynamic_cast<OPSObject*> (value);

            if (opsO != NULL)
            {
                os << tab() << "<" << name << " type = \"" << opsO->getTypeString() << "\" >" << "\n";
                currentTabDepth++;
                value->serialize(this);
                currentTabDepth--;
                os << tab() << "</" << name << ">" << "\n";
            }
            return value;

        }

        virtual void inout(InoutName_T name, Serializable& value)
        {
            UNUSED(name);
            UNUSED(value);
        }

        virtual void inout(InoutName_T name, std::vector<bool>& value)
        {
            os << tab() << "<" << name << ">" << std::endl;
            for (unsigned int i = 0; i < value.size(); i++)
            {
                currentTabDepth++;
                bool e = value[i];
                inout("element", e);
                currentTabDepth--;
            }
            os << tab() << "</" << name << ">" << std::endl;
        }

        virtual void inout(InoutName_T name, std::vector<char>& value)
        {
            os << tab() << "<" << name << ">" << std::endl;
            for (unsigned int i = 0; i < value.size(); i++)
            {
                currentTabDepth++;
                inout("element", value[i]);
                currentTabDepth--;
            }
            os << tab() << "</" << name << ">" << std::endl;
        }

        virtual void inout(InoutName_T name, std::vector<int>& value)
        {
            os << tab() << "<" << name << ">" << std::endl;
            for (unsigned int i = 0; i < value.size(); i++)
            {
                currentTabDepth++;
                inout("element", value[i]);
                currentTabDepth--;
            }
            os << tab() << "</" << name << ">" << std::endl;
        }

        virtual void inout(InoutName_T name, std::vector<int16_t>& value)
        {
            os << tab() << "<" << name << ">" << std::endl;
            for (unsigned int i = 0; i < value.size(); i++)
            {
                currentTabDepth++;
                inout("element", value[i]);
                currentTabDepth--;
            }
            os << tab() << "</" << name << ">" << std::endl;
        }

        virtual void inout(InoutName_T name, std::vector<int64_t>& value)
        {
            os << tab() << "<" << name << ">" << std::endl;
            for (unsigned int i = 0; i < value.size(); i++)
            {
                currentTabDepth++;
                inout("element", value[i]);
                currentTabDepth--;
            }
            os << tab() << "</" << name << ">" << std::endl;
        }

        virtual void inout(InoutName_T name, std::vector<float>& value)
        {
            os << tab() << "<" << name << ">" << std::endl;
            for (unsigned int i = 0; i < value.size(); i++)
            {
                currentTabDepth++;
                inout("element", value[i]);
                currentTabDepth--;
            }
            os << tab() << "</" << name << ">" << std::endl;
        }

        virtual void inout(InoutName_T name, std::vector<double>& value)
        {
            os << tab() << "<" << name << ">" << std::endl;
            for (unsigned int i = 0; i < value.size(); i++)
            {
                currentTabDepth++;
                inout("element", value[i]);
                currentTabDepth--;
            }
            os << tab() << "</" << name << ">" << std::endl;
        }

        virtual void inout(InoutName_T name, std::vector<std::string>& value)
        {
            os << tab() << "<" << name << ">" << std::endl;
            for (unsigned int i = 0; i < value.size(); i++)
            {
                currentTabDepth++;
                inout("element", value[i]);
                currentTabDepth--;
            }
            os << tab() << "</" << name << ">" << std::endl;
        }

		void inoutfixarr(InoutName_T name, bool* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverOut.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, char* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverOut.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, int* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverOut.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, int16_t* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverOut.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, int64_t* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverOut.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, float* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverOut.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, double* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverOut.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, std::string* value, int numElements)
        {
            UNUSED(name)
            UNUSED(value)
            UNUSED(numElements);
            ///TODO
            throw ops::ArchiverException("XMLArchiverOut.inoutfixarr NYI");
        }

        int beginList(InoutName_T name, int size)
        {
            //buf->WriteInt(size);
            os << tab() << "<" << name << ">" << std::endl;
            currentTabDepth++;
            return size;
        }

        void endList(InoutName_T name)
        {
            currentTabDepth--;
            os << tab() << "</" << name << ">" << std::endl;

        }

    };
}
#endif
