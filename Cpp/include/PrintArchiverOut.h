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

#ifndef PrintArchiverOutH
#define PrintArchiverOutH

#include "ArchiverInOut.h"
#include <vector>
#include <string>
#include <iostream>
#include "OPSObject.h"


namespace ops
{
    using namespace std;

    //Pure virtual interface.

    class PrintArchiverOut : public ArchiverInOut
    {
        std::ostream& os;
        int currentTabDepth;
        const static int tabSize = 3;

    private:

        std::string tab()
        {
            std::string ret("");
            for (int i = 0; i < currentTabDepth; i++)
                ret += ("   ");
            return ret;
        }
    public:

        PrintArchiverOut(std::ostream& os_) : os(os_), currentTabDepth(0)
        {

        }

        void close()
        {

        }

        void printObject(const std::string& name, Serializable* obj)
        {
            os << "\n________________Begin Object___________________" << "\n\n";
            inout(name, obj);
            os << "\n_________________End Object____________________" << "\n";
        }

        virtual void inout(const std::string& name, bool& value)
        {

            os << tab() << name << " = " << value << "\n";

        }

        virtual void inout(const std::string& name, char& value)
        {
            os << tab() << name << " = " << (int)value << "\n";
        }

        virtual void inout(const std::string& name, int& value)
        {
            os << tab() << name << " = " << value << "\n";
        }

        virtual void inout(const std::string& name, __int16& value)
        {
            os << tab() << name << " = " << value << "\n";
        }

        virtual void inout(const std::string& name, __int64& value)
        {
            os << tab() << name << " = " << value << "\n";
        }

        virtual void inout(const std::string& name, float& value)
        {
            os << tab() << name << " = " << value << "\n";
        }

        virtual void inout(const std::string& name, double& value)
        {
            os << tab() << name << " = " << value << "\n";
        }

        virtual void inout(const std::string& name, std::string& value)
        {
            os << tab() << name << " = " << value << "\n";
        }

        virtual Serializable* inout(const std::string& name, Serializable* value, int element)
        {
			OPSObject* opsO = dynamic_cast<OPSObject*> (value);
			if (opsO != NULL)
			{
				os << tab() << name << " type = " << opsO->getTypeString() << "\n";
				currentTabDepth++;
				value->serialize(this);
				currentTabDepth--;

			}
            return value;

        }

        virtual Serializable* inout(const std::string& name, Serializable* value)
        {
            OPSObject* opsO = dynamic_cast<OPSObject*> (value);

            if (opsO != NULL)
            {
                os << tab() << name << " type = " << opsO->getTypeString() << "\n";
                currentTabDepth++;
                value->serialize(this);
                currentTabDepth--;

            }
            return value;

        }

        virtual void inout(const std::string& name, Serializable& value)
        {
            OPSObject* opsO = dynamic_cast<OPSObject*> (&value);

            if (opsO != NULL)
            {
                os << tab() << name << " type = " << opsO->getTypeString() << "\n";
                currentTabDepth++;
                value.serialize(this);
                currentTabDepth--;

            }


        }

///LA
        virtual void inout(const std::string& name, char* buffer, int bufferSize)
        {
            ///TODO
        }
///LA

        virtual void inout(const std::string& name, std::vector<bool>& value)
        {
            if (value.size() > 0)
            {
                std::string value0 = value[0] ? "true" : "false";
                std::string valueX = value[value.size() - 1] ? "true" : "false";

                os << tab() << name << "(size = " << value.size() << ") = [ " << value0 << " ... " << valueX << " ]" << endl;
            
            }
        }

        virtual void inout(const std::string& name, std::vector<char>& value)
        {
            if (value.size() > 0)
                os << tab() << name << "(size = " << value.size() << ") = [ " << (int)value[0] << " ... " << (int)value[value.size() - 1] << " ]" << endl;
        }

        virtual void inout(const std::string& name, std::vector<int>& value)
        {
            if (value.size() > 0)
                os << tab() << name << "(size = " << value.size() << ") = [ " << value[0] << " ... " << value[value.size() - 1] << " ]" << endl;
        }

        virtual void inout(const std::string& name, std::vector<__int16>& value)
        {
            if (value.size() > 0)
                os << tab() << name << "(size = " << value.size() << ") = [ " << value[0] << " ... " << value[value.size() - 1] << " ]" << endl;
        }

        virtual void inout(const std::string& name, std::vector<__int64>& value)
        {
            if (value.size() > 0)
                os << tab() << name << "(size = " << value.size() << ") = [ " << value[0] << " ... " << value[value.size() - 1] << " ]" << endl;
        }

        virtual void inout(const std::string& name, std::vector<float>& value)
        {
            if (value.size() > 0)
                os << tab() << name << "(size = " << value.size() << ") = [ " << value[0] << " ... " << value[value.size() - 1] << " ]" << endl;
            //            os << tab() << name << " = [" << endl;
            //            for (unsigned int i = 0; i < value.size(); i++)
            //            {
            //                currentTabDepth++;
            //                float e = value[i];
            //                inout(std::string(""), e);
            //                currentTabDepth--;
            //            }
            //            os << tab() << "]" << endl;
        }

        virtual void inout(const std::string& name, std::vector<double>& value)
        {
            if (value.size() > 0)
                os << tab() << name << "(size = " << value.size() << ") = [ " << value[0] << " ... " << value[value.size() - 1] << " ]" << endl;
        }

        virtual void inout(const std::string& name, std::vector<std::string>& value)
        {
            if (value.size() > 0)
                os << tab() << name << "(size = " << value.size() << ") = [ " << value[0] << " ... " << value[value.size() - 1] << " ]" << endl;
        }

		virtual void inoutfixarr(const std::string& name, bool* value, int numElements, int totalSize)
		{
			std::string value0 = value[0] ? "true" : "false";
			std::string valueX = value[numElements - 1] ? "true" : "false";
			os << tab() << name << "(size = " << numElements << ") = [ " << value0 << " ... " << valueX << " ]" << endl;
		}

		virtual void inoutfixarr(const std::string& name, char* value, int numElements, int totalSize)
		{
			os << tab() << name << "(size = " << numElements << ") = [ " << (int)value[0] << " ... " << (int)value[numElements - 1] << " ]" << endl;
		}

		virtual void inoutfixarr(const std::string& name, int* value, int numElements, int totalSize)
		{
			os << tab() << name << "(size = " << numElements << ") = [ " << value[0] << " ... " << value[numElements - 1] << " ]" << endl;
		}

		virtual void inoutfixarr(const std::string& name, __int16* value, int numElements, int totalSize)
		{
			os << tab() << name << "(size = " << numElements << ") = [ " << value[0] << " ... " << value[numElements - 1] << " ]" << endl;
		}

		virtual void inoutfixarr(const std::string& name, __int64* value, int numElements, int totalSize)
		{
			os << tab() << name << "(size = " << numElements << ") = [ " << value[0] << " ... " << value[numElements - 1] << " ]" << endl;
		}

		virtual void inoutfixarr(const std::string& name, float* value, int numElements, int totalSize)
		{
			os << tab() << name << "(size = " << numElements << ") = [ " << value[0] << " ... " << value[numElements - 1] << " ]" << endl;
		}

		virtual void inoutfixarr(const std::string& name, double* value, int numElements, int totalSize)
		{
			os << tab() << name << "(size = " << numElements << ") = [ " << value[0] << " ... " << value[numElements - 1] << " ]" << endl;
		}

		virtual void inoutfixarr(const std::string& name, std::string* value, int numElements)
		{
			os << tab() << name << "(size = " << numElements << ") = [ " << value[0] << " ... " << value[numElements - 1] << " ]" << endl;
		}
		
		int beginList(const std::string& name, int size)
        {
            os << tab() << name << " = " << endl;
            currentTabDepth++;
            return size;
        }

        void endList(const std::string& name)
        {
            currentTabDepth--;
        }

    };

}
#endif
