///////////////////////////////////////////////////////////
//  ArchiverInOut.cs
//  Implementation of the Interface ArchiverInOut
//  Created on:      12-nov-2011 21:02:39
//  Author:
///////////////////////////////////////////////////////////

using System.Collections;               // Needed for the "ArrayList"
using System.Collections.Generic;       // Needed for the "List"

namespace Ops 
{
	public interface IArchiverInOut 
    {
        /// <summary>
        /// Returns true if archiver serializes objects. Returns false if archiver deserialize byte streams.
        /// </summary>
        /// <returns></returns>
        bool IsOut();  

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
		int Inout(string name, int v);  

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        long Inout(string name, long v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        byte Inout(string name, byte v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        short Inout(string name, short v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        float Inout(string name, float v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        bool Inout(string name, bool v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        string Inout(string name, string v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        double Inout(string name, double v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        ISerializable Inout(string name, ISerializable v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        List<bool> InoutBooleanList(string name, List<bool> v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        List<byte> InoutByteList(string name, List<byte> v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        List<double> InoutDoubleList(string name, List<double> v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        List<float> InoutFloatList(string name, List<float> v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        List<int> InoutIntegerList(string name, List<int> v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        List<long> InoutLongList(string name, List<long> v);

        /// 
        /// <param name="name"></param>
        /// <param name="v"></param>
        IList InoutSerializableList(string name, IList v);
        IList InoutSerializableList<T>(string name, IList v);

        /// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        List<short> InoutShortList(string name, List<short> v);

		/// 
		/// <param name="name"></param>
		/// <param name="v"></param>
        List<string> InoutStringList(string name, List<string> v);
	}

}