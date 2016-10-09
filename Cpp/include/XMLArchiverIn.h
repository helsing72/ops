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

#ifndef XMLArchiverInH
#define XMLArchiverInH

#include "ArchiverInOut.h"
#include "SerializableInheritingTypeFactory.h"
#include <iostream>
#include <sstream>
#include "xml/xmlParser.h"
#include <string>

namespace ops
{
    using namespace std;
    namespace exceptions
    {

        class XMLArchiverException
        {
        private:
            std::string message;
        public:

            XMLArchiverException()
            {
                message = "XMLArchiverException: empty";
            }

            XMLArchiverException(std::string m)
            {
                message = "XMLArchiverException: " + m;
            }

            std::string GetMessage()
            {
                return message;
            }
        };
    }
    using namespace exceptions;

    ///

    class XMLArchiverIn : public ArchiverInOut
    {
    private:
        std::istream& is;
        opsXML::XMLNode currentNode;
        std::string xmlString;
        std::string parseString;
        std::stringstream ss;
    public:

		XMLArchiverIn(std::istream& is_, std::string topNode_, SerializableInheritingTypeFactory* factory) : is(is_)
        {
            this->factory = factory;

            std::string tmp;
            is >> tmp;
            while (!is.eof())
            {
                xmlString += tmp + " ";
                is >> tmp;
            }
            currentNode = opsXML::XMLNode::parseString(xmlString.c_str(), topNode_.c_str());
        }

        ~XMLArchiverIn()
        {
        }

        virtual void inout(const std::string& name, bool& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                string s(currentNode.getChildNode(name.c_str()).getText());
                if (s.compare("true") == 0) value = true;
                if (s.compare("false") == 0) value = false;
                if (s.compare("TRUE") == 0) value = true;
                if (s.compare("FALSE") == 0) value = false;
                if (s.compare("true") == 0) value = true;
                if (s.compare("false") == 0) value = false;
                if (s.compare("True") == 0) value = true;
                if (s.compare("False") == 0) value = false;
            }
        }

        virtual void inout(const std::string& name, char& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                string s(currentNode.getChildNode(name.c_str()).getText());
                stringstream ss(s);

                int inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(const std::string& name, int& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                string s(currentNode.getChildNode(name.c_str()).getText());
                stringstream ss(s);

                int inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(const std::string& name, __int16& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                string s(currentNode.getChildNode(name.c_str()).getText());
                stringstream ss(s);

                int inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(const std::string& name, __int64& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                string s(currentNode.getChildNode(name.c_str()).getText());
                stringstream ss(s);

                __int64 inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(const std::string& name, float& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                string s(currentNode.getChildNode(name.c_str()).getText());
                stringstream ss(s);

                float inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(const std::string& name, double& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                string s(currentNode.getChildNode(name.c_str()).getText());
                stringstream ss(s);

                double inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(const std::string& name, std::string& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                if (currentNode.getChildNode(name.c_str()).getText() != NULL)
                {
                    string s(currentNode.getChildNode(name.c_str()).getText());
                    value = s;
                }
                else
                {
                    value = "";
                }
            }
        }

		virtual void inout(const std::string& name, char* buffer, int bufferSize)
		{
			///TODO
		}

        virtual Serializable* inout(const std::string& name, Serializable* value, int element)
        {
            opsXML::XMLNode tempNode = currentNode;
            currentNode = currentNode.getChildNode(name.c_str()).getChildNode("element", element);
            string types(currentNode.getAttribute("type"));
            Serializable* newSer = factory->create(types);
            if (newSer != NULL)
            {
                newSer->serialize(this);

            }

            currentNode = tempNode; 

            return newSer;
        }

        virtual void inout(const std::string& name, Serializable& value)
        {

        }

        virtual Serializable* inout(const std::string& name, Serializable* value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                opsXML::XMLNode tempNode = currentNode;
                currentNode = currentNode.getChildNode(name.c_str());
                string types(currentNode.getAttribute("type"));
                Serializable* newSer = factory->create(types);
                if (newSer != NULL)
                {
                    newSer->serialize(this);

                }

                currentNode = tempNode;

                return newSer;
            }
            return value;
        }

        virtual void inout(const std::string& name, std::vector<bool>& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                opsXML::XMLNode tempNode = currentNode;
                currentNode = currentNode.getChildNode(name.c_str());

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, false);
                for (int i = 0; i < size; i++)
                {
                    string s(currentNode.getChildNode("element", i).getText());
                    if (s.compare("true") == 0) value[i] = true;
                    if (s.compare("false") == 0) value[i] = false;
                    if (s.compare("TRUE") == 0) value[i] = true;
                    if (s.compare("FALSE") == 0) value[i] = false;
                    if (s.compare("true") == 0) value[i] = true;
                    if (s.compare("false") == 0) value[i] = false;
                    if (s.compare("True") == 0) value[i] = true;
                    if (s.compare("False") == 0) value[i] = false;
                }

                currentNode = tempNode;
            }
        }

        virtual void inout(const std::string& name, std::vector<char>& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                opsXML::XMLNode tempNode = currentNode;
                currentNode = currentNode.getChildNode(name.c_str());

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0);
                for (int i = 0; i < size; i++)
                {
                    string s(currentNode.getChildNode("element", i).getText());
                    stringstream ss(s);

                    int inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

                currentNode = tempNode;
            }
        }

        virtual void inout(const std::string& name, std::vector<int>& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                opsXML::XMLNode tempNode = currentNode;
                currentNode = currentNode.getChildNode(name.c_str());

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0);
                for (int i = 0; i < size; i++)
                {
                    string s(currentNode.getChildNode("element", i).getText());
                    stringstream ss(s);

                    int inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

                currentNode = tempNode;
            }
        }

        virtual void inout(const std::string& name, std::vector<__int16>& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                opsXML::XMLNode tempNode = currentNode;
                currentNode = currentNode.getChildNode(name.c_str());

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0);
                for (int i = 0; i < size; i++)
                {
                    string s(currentNode.getChildNode("element", i).getText());
                    stringstream ss(s);

                    int inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

                currentNode = tempNode;
            }
        }

        virtual void inout(const std::string& name, std::vector<__int64>& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                opsXML::XMLNode tempNode = currentNode;
                currentNode = currentNode.getChildNode(name.c_str());

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0);
                for (int i = 0; i < size; i++)
                {
                    string s(currentNode.getChildNode("element", i).getText());
                    stringstream ss(s);

                    __int64 inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

                currentNode = tempNode;
            }
        }

        virtual void inout(const std::string& name, std::vector<float>& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                opsXML::XMLNode tempNode = currentNode;
                currentNode = currentNode.getChildNode(name.c_str());

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0.0);
                for (int i = 0; i < size; i++)
                {
                    string s(currentNode.getChildNode("element", i).getText());
                    stringstream ss(s);

                    float inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

                currentNode = tempNode;
            }
        }

        virtual void inout(const std::string& name, std::vector<double>& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                opsXML::XMLNode tempNode = currentNode;
                currentNode = currentNode.getChildNode(name.c_str());

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0.0);
                for (int i = 0; i < size; i++)
                {
                    string s(currentNode.getChildNode("element", i).getText());
                    stringstream ss(s);

                    double inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

                currentNode = tempNode;
            }
        }

        virtual void inout(const std::string& name, std::vector<std::string>& value)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                opsXML::XMLNode tempNode = currentNode;
                currentNode = currentNode.getChildNode(name.c_str());

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, "");
                for (int i = 0; i < size; i++)
                {
                    string s(currentNode.getChildNode("element", i).getText());
                    value[i] = s;
                }

                currentNode = tempNode;
            }
        }

        int beginList(const std::string& name, int size)
        {
            if (!currentNode.getChildNode(name.c_str()).isEmpty())
            {
                return currentNode.getChildNode(name.c_str()).nChildNode("element");
            }
            return 0;
        }

        void endList(const std::string& name)
        {
            //Nothing to do in this implementation
        }

    private:
        SerializableInheritingTypeFactory* factory;

    };
}

#endif
