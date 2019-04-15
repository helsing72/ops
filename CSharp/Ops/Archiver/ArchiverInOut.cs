///////////////////////////////////////////////////////////
//  ArchiverInOut.cs
//  Implementation of the Interface ArchiverInOut
//  Created on:      12-nov-2011 21:02:39
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections;               // Needed for the "ArrayList"
using System.Collections.Generic;       // Needed for the "List"

namespace Ops 
{
    public abstract class IArchiverInOut
    {
        /// <summary>
        /// Returns true if archiver serializes objects. Returns false if archiver deserialize byte streams.
        /// </summary>
        /// <returns></returns>
        public abstract bool IsOut();

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract int Inout(string name, int v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract long Inout(string name, long v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract byte Inout(string name, byte v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract short Inout(string name, short v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract float Inout(string name, float v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract bool Inout(string name, bool v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract string Inout(string name, string v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract double Inout(string name, double v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public T InoutEnum<T>(string name, T v) where T : System.Enum
        {
            short tmp = Convert.ToInt16(v);
            tmp = Inout(name, tmp);
            return (T)(object)tmp;
        }

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract ISerializable Inout(string name, ISerializable v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract List<bool> InoutBooleanList(string name, List<bool> v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract List<byte> InoutByteList(string name, List<byte> v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract List<double> InoutDoubleList(string name, List<double> v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract List<float> InoutFloatList(string name, List<float> v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract List<int> InoutIntegerList(string name, List<int> v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract List<long> InoutLongList(string name, List<long> v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public List<T> InoutEnumList<T>(string name, List<T> v) where T : System.Enum
        {
            int size = Inout(name, v.Count);

            if (IsOut()) {
                foreach (T et in v) {
                    InoutEnum<T>(name, et);
                }
                return v;
            }
            else
            {
                Type type = v.GetType().GetGenericArguments()[0];
                IList list = (IList)Activator.CreateInstance((typeof(List<>).MakeGenericType(type)));
                for (int i = 0; i < size; i++) {
                    short tmp = 0;
                    tmp = Inout(name, tmp);
                    list.Add((T)(object)tmp);
                }
                return (List<T>)list;
            }
        }

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract IList InoutSerializableList(string name, IList v);
        public abstract IList InoutSerializableList<T>(string name, IList v);

        /// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        public abstract List<short> InoutShortList(string name, List<short> v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        public abstract List<string> InoutStringList(string name, List<string> v);
	}

}