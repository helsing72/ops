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

#include <iostream>
#include <sstream>
#include <string>
#include <stack>
#include <exception>

#include "OPSTypeDefs.h"
#include "ArchiverInOut.h"
#include "SerializableInheritingTypeFactory.h"
#include "xml/xmlParser.h"

namespace ops
{
    namespace exceptions
    {
        class XMLArchiverException : public std::exception
        {
        private:
			ExceptionMessage_T message;
        public:
            XMLArchiverException()
            {
                message = "XMLArchiverException: empty";
            }
            XMLArchiverException(ExceptionMessage_T m)
            {
				message = "XMLArchiverException: ";
				message += m;
            }
			const char* what() const NOEXCEPT { return message.c_str(); }
		};
    }
    using namespace exceptions;

#ifdef USE_FIXED_LENGTH_STRINGS
#define NAME(x) x
#else
#define NAME(x) x.c_str()
#endif

    class XMLArchiverIn : public ArchiverInOut
    {
    private:
        std::istream& is;
        opsXML::XMLNode currentNode;
        std::string xmlString;
        std::string parseString;
        std::stringstream ss;

		std::stack<opsXML::XMLNode> _stack;

		void PushNode(opsXML::XMLNode& node) 
		{
			_stack.push(node);
		}

		void PopNode(opsXML::XMLNode& node)
		{
			if (_stack.size() == 0) throw ops::ArchiverException("XMLArchiverIn: Mismatched Push/Pop");
			node = _stack.top();
			_stack.pop();
		}

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

		// Returns true if it's an output archiver
		virtual bool isOut() { return false; }

		virtual void inout(InoutName_T name, bool& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
                std::string s(currentNode.getChildNode(NAME(name)).getText());
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

        virtual void inout(InoutName_T name, char& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				std::string s(currentNode.getChildNode(NAME(name)).getText());
                std::stringstream ss(s);

                int inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(InoutName_T name, int& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
                std::string s(currentNode.getChildNode(NAME(name)).getText());
                std::stringstream ss(s);

                int inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(InoutName_T name, int16_t& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
                std::string s(currentNode.getChildNode(NAME(name)).getText());
				std::stringstream ss(s);

                int inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(InoutName_T name, int64_t& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				std::string s(currentNode.getChildNode(NAME(name)).getText());
				std::stringstream ss(s);

                int64_t inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(InoutName_T name, float& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				std::string s(currentNode.getChildNode(NAME(name)).getText());
				std::stringstream ss(s);

                float inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(InoutName_T name, double& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				std::string s(currentNode.getChildNode(NAME(name)).getText());
				std::stringstream ss(s);

                double inVal;
                ss >> inVal;
                value = inVal;
            }
        }

        virtual void inout(InoutName_T name, std::string& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
                if (currentNode.getChildNode(NAME(name)).getText() != NULL)
                {
					std::string s(currentNode.getChildNode(NAME(name)).getText());
                    value = s;
                }
                else
                {
                    value = "";
                }
            }
        }

		virtual void inoutfixstring(InoutName_T name, char* value, int& size, int max_size, int idx)
		{
			if (!currentNode.getChildNode(NAME(name), idx).isEmpty())
			{
				if (currentNode.getChildNode(NAME(name), idx).getText() != NULL)
				{
					std::string s(currentNode.getChildNode(NAME(name), idx).getText());
					int len = (int)s.size();
					if (len > max_size) throw ops::ArchiverException("Illegal size of fix string received. name: ", name);
					if (len > 0) memcpy(value, s.c_str(), len);
					size = len;
				}
				else
				{
					size = 0;
				}
				value[size] = '\0';
			}
		}
		
		virtual void inout(InoutName_T name, char* buffer, int bufferSize)
        {
            UNUSED(name);
            UNUSED(buffer);
            UNUSED(bufferSize);
            ///TODO
            throw ops::ArchiverException("XMLArchiverIn.inout(name, char*, int) NYI");
        }

        virtual Serializable* inout(InoutName_T name, Serializable* value, int element)
        {
            UNUSED(value);

            PushNode(currentNode);
            currentNode = currentNode.getChildNode("element", element);
			TypeId_T types(currentNode.getAttribute("type"));
            Serializable* newSer = factory->create(types);
            if (newSer != NULL)
            {
                newSer->serialize(this);
            }

			PopNode(currentNode);

            return newSer;
        }

        virtual void inout(InoutName_T name, Serializable& value)
        {
            UNUSED(name);
            UNUSED(value);
        }

        virtual Serializable* inout(InoutName_T name, Serializable* value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				PushNode(currentNode);
				currentNode = currentNode.getChildNode(NAME(name));
				TypeId_T types(currentNode.getAttribute("type"));
                Serializable* newSer = factory->create(types);
                if (newSer != NULL)
                {
                    newSer->serialize(this);
                }

				PopNode(currentNode);

                return newSer;
            }
            return value;
        }

        virtual void inout(InoutName_T name, std::vector<bool>& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				PushNode(currentNode);
				currentNode = currentNode.getChildNode(NAME(name));

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, false);
                for (int i = 0; i < size; i++)
                {
					std::string s(currentNode.getChildNode("element", i).getText());
                    if (s.compare("true") == 0) value[i] = true;
                    if (s.compare("false") == 0) value[i] = false;
                    if (s.compare("TRUE") == 0) value[i] = true;
                    if (s.compare("FALSE") == 0) value[i] = false;
                    if (s.compare("true") == 0) value[i] = true;
                    if (s.compare("false") == 0) value[i] = false;
                    if (s.compare("True") == 0) value[i] = true;
                    if (s.compare("False") == 0) value[i] = false;
                }

				PopNode(currentNode);
			}
        }

        virtual void inout(InoutName_T name, std::vector<char>& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				PushNode(currentNode);
				currentNode = currentNode.getChildNode(NAME(name));

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0);
                for (int i = 0; i < size; i++)
                {
					std::string s(currentNode.getChildNode("element", i).getText());
					std::stringstream ss(s);

                    int inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

				PopNode(currentNode);
			}
        }

        virtual void inout(InoutName_T name, std::vector<int>& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				PushNode(currentNode);
				currentNode = currentNode.getChildNode(NAME(name));

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0);
                for (int i = 0; i < size; i++)
                {
					std::string s(currentNode.getChildNode("element", i).getText());
					std::stringstream ss(s);

                    int inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

				PopNode(currentNode);
			}
        }

        virtual void inout(InoutName_T name, std::vector<int16_t>& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				PushNode(currentNode);
				currentNode = currentNode.getChildNode(NAME(name));

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0);
                for (int i = 0; i < size; i++)
                {
					std::string s(currentNode.getChildNode("element", i).getText());
					std::stringstream ss(s);

                    int inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

				PopNode(currentNode);
			}
        }

        virtual void inout(InoutName_T name, std::vector<int64_t>& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				PushNode(currentNode);
				currentNode = currentNode.getChildNode(NAME(name));

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0);
                for (int i = 0; i < size; i++)
                {
					std::string s(currentNode.getChildNode("element", i).getText());
					std::stringstream ss(s);

                    int64_t inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

				PopNode(currentNode);
			}
        }

        virtual void inout(InoutName_T name, std::vector<float>& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				PushNode(currentNode);
				currentNode = currentNode.getChildNode(NAME(name));

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0.0);
                for (int i = 0; i < size; i++)
                {
					std::string s(currentNode.getChildNode("element", i).getText());
					std::stringstream ss(s);

                    float inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

				PopNode(currentNode);
			}
        }

        virtual void inout(InoutName_T name, std::vector<double>& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				PushNode(currentNode);
				currentNode = currentNode.getChildNode(NAME(name));

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, 0.0);
                for (int i = 0; i < size; i++)
                {
					std::string s(currentNode.getChildNode("element", i).getText());
					std::stringstream ss(s);

                    double inVal;
                    ss >> inVal;
                    value[i] = inVal;
                }

				PopNode(currentNode);
			}
        }

        virtual void inout(InoutName_T name, std::vector<std::string>& value)
        {
            if (!currentNode.getChildNode(NAME(name)).isEmpty())
            {
				PushNode(currentNode);
				currentNode = currentNode.getChildNode(NAME(name));

                int size = currentNode.nChildNode("element");
                value.reserve(size);
                value.resize(size, "");
                for (int i = 0; i < size; i++)
                {
					std::string s(currentNode.getChildNode("element", i).getText());
                    value[i] = s;
                }

				PopNode(currentNode);
			}
        }

		void inoutfixarr(InoutName_T name, bool* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverIn.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, char* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverIn.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, int* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverIn.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, int16_t* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverIn.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, int64_t* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverIn.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, float* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverIn.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, double* value, int numElements, int totalSize)
		{
			UNUSED(name)
			UNUSED(value)
			UNUSED(numElements);
			UNUSED(totalSize);
			///TODO
			throw ops::ArchiverException("XMLArchiverIn.inoutfixarr NYI");
		}

		void inoutfixarr(InoutName_T name, std::string* value, int numElements)
        {
            UNUSED(name)
            UNUSED(value)
            UNUSED(numElements);
            ///TODO
            throw ops::ArchiverException("XMLArchiverIn.inoutfixarr NYI");
        }

        int beginList(InoutName_T name, int size)
        {
            UNUSED(size);
			PushNode(currentNode);
			currentNode = currentNode.getChildNode(NAME(name));
			
			if (!currentNode.isEmpty())
            {
                return currentNode.nChildNode("element");
            }
            return 0;
        }

        void endList(InoutName_T name)
        {
            UNUSED(name);
			PopNode(currentNode);
		}

    private:
        SerializableInheritingTypeFactory* factory;

    };
}
#endif
