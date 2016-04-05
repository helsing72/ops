///////////////////////////////////////////////////////////
//  OPSArchiverOut.cs
//  Implementation of the Class OPSArchiverOut
//  Created on:      12-nov-2011 09:25:32
//  Author:
///////////////////////////////////////////////////////////

using System.Collections;
using System.Collections.Generic;

namespace Ops
{
	public class OPSArchiverOut : IArchiverInOut 
    {
		internal WriteByteBuffer writeBuf;

		public OPSArchiverOut(WriteByteBuffer buf)
        {
            this.writeBuf = buf;
        }

        public bool IsOut()
        {
            return true;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public int Inout(string name, int v)
        {
            writeBuf.Write(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public long Inout(string name, long v)
        {
            writeBuf.Write(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public byte Inout(string name, byte v)
        {
            writeBuf.Write(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public short Inout(string name, short v)
        {
            writeBuf.Write(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public float Inout(string name, float v)
        {
            writeBuf.Write(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public bool Inout(string name, bool v)
        {
            writeBuf.Write(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public string Inout(string name, string v)
        {
            writeBuf.Write(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public double Inout(string name, double v)
        {
            writeBuf.Write(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public ISerializable Inout(string name, ISerializable v)
        {
            writeBuf.Write(((OPSObject)v).GetTypesString());
            v.Serialize(this);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public List<bool> InoutBooleanList(string name, List<bool> v)
        {
            writeBuf.WriteBooleanArr(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public List<byte> InoutByteList(string name, List<byte> v)
        {
            writeBuf.WriteByteArr(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public List<double> InoutDoubleList(string name, List<double> v)
        {
            writeBuf.WriteDoubleArr(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public List<float> InoutFloatList(string name, List<float> v)
        {
            writeBuf.WriteFloatArr(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public List<int> InoutIntegerList(string name, List<int> v)
        {
            writeBuf.WriteIntArr(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public List<long> InoutLongList(string name, List<long> v)
        {
            writeBuf.WriteLongArr(v);
            return v;
        }

        // NB! we assume that the object is a List<X> where X implements ISerializable.
        public IList InoutSerializableList(string name, IList v)
        {
            writeBuf.Write(((IList)v).Count);
            foreach (ISerializable obj in (IList)v)
            {
                writeBuf.Write(((OPSObject)obj).GetTypesString());
                obj.Serialize(this);
            }
            return v;
        }

        // NB! we assume that the object is a List<T> where T implements ISerializable.
        public IList InoutSerializableList<T>(string name, IList v)
        {
            return InoutSerializableList(name, v);
        }

        /// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public List<short> InoutShortList(string name, List<short> v)
        {
            writeBuf.WriteShortArr(v);
            return v;
        }

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		public List<string> InoutStringList(string name, List<string> v)
        {
            writeBuf.WriteStringArr(v);
            return v;
        }

	}

}