///////////////////////////////////////////////////////////
//  XMLArchiverIn.cs
//  Implementation of the Class XMLArchiverIn
//  Created on:      12-nov-2011 21:02:41
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Xml;

namespace Ops
{
	public class XMLArchiverIn : IArchiverInOut 
    {
        private const NumberStyles numberStyles = NumberStyles.AllowLeadingSign | NumberStyles.AllowDecimalPoint | NumberStyles.AllowExponent | NumberStyles.AllowLeadingWhite | NumberStyles.AllowTrailingWhite;
        private NumberFormatInfo numberFormatInfo = new NumberFormatInfo() { NumberDecimalSeparator = "." };
		private XmlNode currentNode;
        private XmlDocument doc = new XmlDocument();
		private SerializableCompositeFactory compositeFactory = new SerializableCompositeFactory();
		private Stack<XmlNode> nodeStack = new Stack<XmlNode>();

        public XMLArchiverIn(Stream s)
        {
            try
            {
                // Load the XML data from a file.
                doc.Load(s);                      

                currentNode = doc;
             } 
             catch(XmlException xmlEx)
             {
                throw new FormatException("Caused by underlying XML exception: " + xmlEx.Message);
             }
             catch(Exception ex)
             {
                throw new FormatException("Caused by underlying generic exception: " + ex.Message);
             }
             finally
             {
                // Finalize here.
             }
        }

        public XMLArchiverIn(Stream s, string rootNode)
        {
            try
            {
                // Load the XML data from a file.
                doc.Load(s);                      

                currentNode = doc;
                currentNode = GetNode(rootNode);
            }
             catch(XmlException xmlEx)
             {
                throw new FormatException("Caused by underlying XML exception: " + xmlEx.Message);
             }
             catch(Exception ex)
             {
                throw new FormatException("Caused by underlying generic exception: " + ex.Message);
             }
             finally
             {
                // Finalize here.
             }
        }

        public bool IsOut()
        {
            return false;
        }

        public int GetNrElements(string name)
        {
            nodeStack.Push(currentNode);
            try
            {
                currentNode = GetNode(name);
                if (currentNode == null)
                {
                    return 0;
                }
                XmlNodeList nodes = currentNode.ChildNodes;
                int size = 0;
                foreach (XmlNode node in nodes)
                {
                    if (node.Name.Equals("element"))
                    {
                        size++;
                    }
                }
                return size;
            }
            finally
            {
                currentNode = nodeStack.Pop();
            }
        }

        private XmlNode GetCurrentElement(int n)
        {
            int ittElement = 0;
            XmlNodeList nodes = currentNode.ChildNodes;
            foreach (XmlNode node in nodes)
            {
                if (node.Name.Equals("element") && n == ittElement)
                {
                    return node;
                } 
                else
                {
                    if (node.Name.Equals("element"))
                    {
                        ittElement++;
                    }
                }
            }
            return null;
        }

        private XmlNode GetNode(string name)
        {
            XmlNodeList nodes = currentNode.ChildNodes;
            foreach (XmlNode node in nodes)
            {
                if (node.Name.Equals(name))
                {
                    return node;
                }
            }
            return null;
        }

        private string GetValue(string name)
        {
            XmlNodeList nodes = currentNode.ChildNodes;
            foreach (XmlNode node in nodes)
            {
                if (node.Name.Equals(name))
                {
                    return node.InnerText.Trim();
                }
            }
            return null;
        }

        // Read a "bool" value with name "name". If not found return the input value "v".
        public bool Inout(string name, bool v)
        {
            bool ret;
            if (bool.TryParse(GetValue(name), out ret))
            {
                return ret;
            }
            else
            {
                return v;
            }
        }

        // Read a "byte" value with name "name". If not found return the input value "v".
        public byte Inout(string name, byte v)
        {
            byte ret;
            if (byte.TryParse(GetValue(name), out ret))
            {
                return ret;
            }
            else
            {
                return v;
            }
        }

        // Read a "double" value with name "name". If not found return the input value "v".
        public double Inout(string name, double v)
        {
            double ret;

            // A specified NumberFormatInfo ensures that values, regardless of regional format settings in Windows, 
            // always are read with "." as decimal separator
            if (double.TryParse(GetValue(name), numberStyles, numberFormatInfo, out ret))
            {
                return ret;
            }
            else
            {
                return v;
            }
        }

        // Read a "float" value with name "name". If not found return the input value "v".
        public float Inout(string name, float v)
        {
            float ret;

            // A specified NumberFormatInfo ensures that values, regardless of regional format settings in Windows, 
            // always are read with "." as decimal separator
            if (float.TryParse(GetValue(name), numberStyles, numberFormatInfo, out ret))
            {
                return ret;
            }
            else
            {
                return v;
            }
        }

        // Read an "int" value with name "name". If not found return the input value "v".
        public int Inout(string name, int v) 
        {
            int ret;
            if (int.TryParse(GetValue(name), out ret))
            {
                return ret;
            }
            else
            {
                return v;
            }
        }

        // Read a "long" value with name "name". If not found return the input value "v".
        public long Inout(string name, long v)
        {
            long ret;
            if (long.TryParse(GetValue(name), out ret))
            {
                return ret;
            }
            else
            {
                return v;
            }
        }

        // Read a "short" value with name "name". If not found return the input value "v".
        public short Inout(string name, short v)
        {
            short ret;
            if (short.TryParse(GetValue(name), out ret))
            {
                return ret;
            }
            else
            {
                return v;
            }
        }

        // Read a "string" value with name "name". If not found return an empty string.
        public string Inout(string name, string v)
        {
            string ret = GetValue(name);
            if (ret == null)
            {
                ret = "";
            }
            return ret;
        }

        // Read a "ISerializable" item with name "name". 
        public ISerializable Inout(string name, ISerializable v) 
        {
            nodeStack.Push(currentNode);
            currentNode = GetNode(name);
            string type = currentNode.Attributes.GetNamedItem("type").Value;
            ISerializable newElem = compositeFactory.Create(type);
            newElem.Serialize(this);
            currentNode = nodeStack.Pop();
            return newElem;
        }

        public ISerializable GetElement(string name, int i)
        {
            try
            {
                nodeStack.Push(currentNode);
                currentNode = GetNode(name);
                nodeStack.Push(currentNode);
                currentNode = GetCurrentElement(i);
                string type = currentNode.Attributes.GetNamedItem("type").Value;

                ISerializable newElem = compositeFactory.Create(type);

                newElem.Serialize(this);

                currentNode = nodeStack.Pop();
                currentNode = nodeStack.Pop();

                return newElem;
            } 
            catch (Exception)
            {
                return null;
            }
        }

        public void Remove(ISerializableFactory item)
        {
            compositeFactory.Remove(item);
        }

        public void Add(ISerializableFactory item)
        {
            compositeFactory.Add(item);
        }

        delegate bool fn<T>(string input, out T res);

        List<T> InoutCoretypeList<T>(string name, List<T> v, fn<T> Conv)
        {
            int size = GetNrElements(name);

            nodeStack.Push(currentNode);
            currentNode = GetNode(name);

            v.Clear();

            for (int i = 0; i < size; i++)
            {
                XmlNode node = GetCurrentElement(i);
                T res;
                if (Conv(node.InnerText.Trim(), out res))
                {
                    v.Add(res);
                }
                else
                {
                    throw new System.FormatException("Invalid value encountered in Xml-list. Name: '" + name + "', Value: " + node.InnerText.Trim());
                }
            }

            currentNode = nodeStack.Pop();

            return v;
        }

        public List<int> InoutIntegerList(string name, List<int> v)
        {
            return InoutCoretypeList<int>(name, v,
                (string input, out int res) => { return int.TryParse(input, out res); });
        }

        public List<long> InoutLongList(string name, List<long> v) 
        {
            return InoutCoretypeList<long>(name, v,
                (string input, out long res) => { return long.TryParse(input, out res); });
        }

        public List<Byte> InoutByteList(string name, List<Byte> v) 
        {
            return InoutCoretypeList<Byte>(name, v,
                (string input, out Byte res) => { return Byte.TryParse(input, out res); });
        }

        public List<short> InoutShortList(string name, List<short> v) 
        {
            return InoutCoretypeList<short>(name, v,
                (string input, out short res) => { return short.TryParse(input, out res); });
        }

        public List<float> InoutFloatList(string name, List<float> v) 
        {
            return InoutCoretypeList<float>(name, v,
                (string input, out float res) => { return float.TryParse(input, numberStyles, numberFormatInfo, out res); });
        }

        public List<bool> InoutBooleanList(string name, List<bool> v) 
        {
            return InoutCoretypeList<bool>(name, v,
                (string input, out bool res) => { return bool.TryParse(input, out res); });
        }

        public List<string> InoutStringList(string name, List<string> v) 
        {
            return InoutCoretypeList<string>(name, v,
                (string input, out string res) => { res = input; return true; });
        }

        public List<double> InoutDoubleList(string name, List<double> v) 
        {
            return InoutCoretypeList<double>(name, v, 
                (string input, out double res) => { return double.TryParse(input, numberStyles, numberFormatInfo, out res); } );
        }

        // NB! We assume that object "v" is a generic list.
        public IList InoutSerializableList(string name, IList v)
        {
            Type type = v.GetType().GetGenericArguments()[0];
            IList list = (IList)Activator.CreateInstance((typeof(List<>).MakeGenericType(type)));
            int size = GetNrElements(name);
            for (int i = 0; i < size; i++)
            {
                list.Add((ISerializable)GetElement(name, i));
            }
            return list;
        }

        // NB! We assume that object "v" is a generic list.
        public IList InoutSerializableList<T>(string name, IList v)
        {
            return InoutSerializableList(name, v);
        }

    }

}