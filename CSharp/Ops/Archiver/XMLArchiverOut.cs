///////////////////////////////////////////////////////////
//  XMLArchiverOut.cs
//  Implementation of the Class XMLArchiverOut
//  Created on:      12-nov-2011 21:02:41
//  Author:
///////////////////////////////////////////////////////////

using System.Collections;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Text;

namespace Ops
{
	public class XMLArchiverOut : IArchiverInOut 
    {
        private NumberFormatInfo numberFormatInfo = new NumberFormatInfo() { NumberDecimalSeparator = "." };
        private int currentTabDepth = 0;
		private static string HEADER = "<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\" ?>" + LINE_CHANGE;
		private static string LINE_CHANGE = "\n";
		internal BinaryWriter bw = null;
        private Encoding encoding = new UTF8Encoding();
        //private string rootTag;
		private static string TAB = "    ";
		private static string TYPE_ADD_BOOLEAN = " type = \"boolean\" ";
		private static string TYPE_ADD_BYTE = " type = \"byte\" ";
		private static string TYPE_ADD_CLASS = " type = ";
		private static string TYPE_ADD_DOUBLE = " type = \"double\" ";
		private static string TYPE_ADD_FLOAT = " type = \"float\" ";
		private static string TYPE_ADD_INT = " type = \"int\" ";
		private static string TYPE_ADD_LONG = " type = \"long\" ";
		private static string TYPE_ADD_SHORT = " type = \"short\" ";
		private static string TYPE_ADD_STRING = " type = \"string\" ";
		private bool writeXMLHeader = true;
		private bool writType = true;
        private SerializableCompositeFactory compositeFactory = new SerializableCompositeFactory();

        public XMLArchiverOut()
        {
        }

        public XMLArchiverOut(BinaryWriter bw) : this(bw, false)
        {
            
        }

        public XMLArchiverOut(BinaryWriter bw, bool writeXMLHeader)
        {
            this.bw = bw;
            this.writeXMLHeader = writeXMLHeader;
            if (writeXMLHeader)
            {
                bw.Write(encoding.GetBytes(HEADER));
            }
            
        }

        public XMLArchiverOut(BinaryWriter bw, bool writeXMLHeader, string rootTag)
        {
            this.bw = bw;
            this.writeXMLHeader = writeXMLHeader;
            //this.rootTag = rootTag;
            if (writeXMLHeader)
            {
                bw.Write(encoding.GetBytes(HEADER));
            }

        }

        public bool IsOut()
        {
            return true;
        }

        public void SetBinaryWriter(BinaryWriter bw)
        {
            this.bw = bw;
            if (writeXMLHeader)
            {
                bw.Write(encoding.GetBytes(HEADER));
            }
        }
        
        public void Put(string name, byte v)
        {
            string typeAdd = writType ? TYPE_ADD_BYTE : "";
            PutXMLLeaf(name, "" + v, typeAdd);        
        }

        public void Put(string name, short v) 
        {
            string typeAdd = writType ? TYPE_ADD_SHORT : "";
            PutXMLLeaf(name, "" + v, typeAdd); 
        }

        public void Put(string name, int v) 
        {
            string typeAdd = writType ? TYPE_ADD_INT : "";
            PutXMLLeaf(name, "" + v, typeAdd); 
        }

        public void Put(string name, long v) 
        {
            string typeAdd = writType ? TYPE_ADD_LONG : "";
            PutXMLLeaf(name, "" + v, typeAdd); 
        }

        public void Put(string name, float v) 
        {
           string typeAdd = writType ? TYPE_ADD_FLOAT : "";

           // A specified NumberFormatInfo ensures that values, regardless of regional format settings in Windows, 
           // always are written with "." as decimal separator
           PutXMLLeaf(name, "" + v.ToString(numberFormatInfo), typeAdd);  
        }

        public void Put(string name, double v) 
        {
            string typeAdd = writType ? TYPE_ADD_DOUBLE : "";

            // A specified NumberFormatInfo ensures that values, regardless of regional format settings in Windows, 
            // always are written with "." as decimal separator
            PutXMLLeaf(name, "" + v.ToString(numberFormatInfo), typeAdd);
        }

        public void Put(string name, string v) 
        {
            string typeAdd = writType ? TYPE_ADD_STRING : "";
            PutXMLLeaf(name, "" + v, typeAdd); 
        }

        public bool GetWriteXMLHeader()
        {
            return writeXMLHeader;
        }

        public void SetWriteXMLHeader(bool writeXMLHeader)
        {
            this.writeXMLHeader = writeXMLHeader;
        }

        public void SetWriteTypes(bool b)
        {
            writType = b;
        }

        private void PutXMLLeaf(string name, string value, string type) 
        {
            string nodeString = Tab(currentTabDepth) + "<" + name + type + ">" + value + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));               
        }

        private string Tab(int i)
        {
            string ret = "";
            for (int j = 0; j < i; j++)
            {
                ret += TAB;
                
            }
            return ret;
        }

        public void PutBytes(string name, List<byte> collection) 
        {
            string typeAdd = writType ?  TYPE_ADD_CLASS + "\"" + "list" + "\"" : "";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd +">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;

            foreach (byte value in collection)
            {
                Put("element", value);
            }
            currentTabDepth --;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
        }

        public void PutInts(string name, List<int> collection) 
        {
            string typeAdd = writType ?  TYPE_ADD_CLASS + "\"" + "list" + "\"" : "";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd +">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;

            foreach (int value in collection)
            {
                Put("element", value);
            }
            currentTabDepth --;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
        }

        public void PutDoubles(string name, List<double> collection) 
        {
            string typeAdd = writType ?  TYPE_ADD_CLASS + "\"" + "list" + "\"" : "";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd +">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;

            foreach (double value in collection)
            {
                Put("element", value);
            }
            currentTabDepth --;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
        }

        public void PutFloats(string name, List<float> collection) 
        {
            string typeAdd = writType ?  TYPE_ADD_CLASS + "\"" + "list" + "\"" : "";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd +">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;

            foreach (float value in collection)
            {
                Put("element", value);
            }
            currentTabDepth --;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
        }

        public void PutShorts(string name, List<short> collection) 
        {
            string typeAdd = writType ?  TYPE_ADD_CLASS + "\"" + "list" + "\"" : "";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd +">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;

            foreach (short value in collection)
            {
                Put("element", value);
            }
            currentTabDepth --;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
        }

        public void PutLongs(string name, List<long> collection) 
        {
            string typeAdd = writType ?  TYPE_ADD_CLASS + "\"" + "list" + "\"" : "";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd +">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;

            foreach (long value in collection)
            {
                Put("element", value);
            }
            currentTabDepth --;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
        }

        public void PutStrings(string name, List<string> collection) 
        {
            string typeAdd = writType ?  TYPE_ADD_CLASS + "\"" + "list" + "\"" : "";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd +">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;

            foreach (string value in collection)
            {
                Put("element", value);
            }
            currentTabDepth --;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
        }

        public void Put(string name, bool v) 
        {
            string typeAdd = writType ? TYPE_ADD_BOOLEAN : "";
            PutXMLLeaf(name, "" + v, typeAdd);
        }

        public int Inout(string name, int v) 
        {
            Put(name, v);
            return v;
        }

        public long Inout(string name, long v) 
        {
            Put(name, v);
            return v;
        }

        public byte Inout(string name, byte v) 
        {
            Put(name, v);
            return v;
        }

        public short Inout(string name, short v) 
        {
            Put(name, v);
            return v;
        }

        public float Inout(string name, float v) 
        {
            Put(name, v);
            return v;
        }

        public bool Inout(string name, bool v) 
        {
            Put(name, v);
            return v;
        }

        public string Inout(string name, string v) 
        {
            Put(name, v);
            return v;
        }

        public double Inout(string name, double v) 
        {
            Put(name, v);
            return v;
        }

        public ISerializable Inout(string name, ISerializable v) 
        {
            string typeName = compositeFactory.Create(v);

            if (typeName == null)
            {
                typeName = v.GetType().Name;
            }

            string typeAdd = TYPE_ADD_CLASS + "\"" + typeName + "\"";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;
            v.Serialize(this);
            currentTabDepth --;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            return v;
        }

        public List<object> Inout(string name, List<object> v) 
        {
            string typeAdd = writType ?  TYPE_ADD_CLASS + "\"" + "list" + "\"" : "";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd +">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;

            foreach (object value in v)
            {
                Inout("element", (ISerializable)value);
            }
            currentTabDepth --;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            return v;
        }

        public List<int> InoutIntegerList(string name, List<int> v) 
        {
            throw new System.NotSupportedException("Not supported yet.");
        }

        public List<long> InoutLongList(string name, List<long> v) 
        {
            throw new System.NotSupportedException("Not supported yet.");
        }

        public List<byte> InoutByteList(string name, List<byte> v) 
        {
            throw new System.NotSupportedException("Not supported yet.");
        }

        public List<short> InoutShortList(string name, List<short> v) 
        {
            throw new System.NotSupportedException("Not supported yet.");
        }

        public List<float> InoutFloatList(string name, List<float> v) 
        {
            throw new System.NotSupportedException("Not supported yet.");
        }

        public List<bool> InoutBooleanList(string name, List<bool> v) 
        {
            throw new System.NotSupportedException("Not supported yet.");
        }

        public List<string> InoutStringList(string name, List<string> v) 
        {
            throw new System.NotSupportedException("Not supported yet.");
        }

        public List<double> InoutDoubleList(string name, List<double> v) 
        {
            throw new System.NotSupportedException("Not supported yet.");
        }

        public void Remove(ISerializableFactory item)
        {
            compositeFactory.Remove(item);
        }

        public void Add(ISerializableFactory item)
        {
            compositeFactory.Add(item);
        }

        // NB! we assume that the object is a List<X> where X implements ISerializable.
        public IList InoutSerializableList(string name, IList v)
        {
            string typeAdd = writType ? TYPE_ADD_CLASS + "\"" + "list" + "\"" : "";
            string nodeString = Tab(currentTabDepth) + "<" + name + typeAdd + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            currentTabDepth++;

            foreach (ISerializable obj in (IList)v)
            {
                Inout("element", obj);
            }
            currentTabDepth--;
            nodeString = Tab(currentTabDepth) + "</" + name + ">" + LINE_CHANGE;
            bw.Write(encoding.GetBytes(nodeString));
            return v;
        }

        // NB! we assume that the object is a List<T> where T implements ISerializable.
        public IList InoutSerializableList<T>(string name, IList v)
        {
            return InoutSerializableList(name, v);
        }
    }

}