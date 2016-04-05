///////////////////////////////////////////////////////////
//  SerializableCompositeFactory.cs
//  Implementation of the Class SerializableCompositeFactory
//  Created on:      12-nov-2011 21:02:40
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections;
using System.Collections.Generic;

namespace Ops
{
	public class SerializableCompositeFactory : ISerializableFactory 
    {
        internal List<ISerializableFactory> childFactories = new List<ISerializableFactory>();

        /// 
        /// <param name="item"></param>
		public void Add(ISerializableFactory item)
        {
            childFactories.Add(item);
        }

		/// 
		/// <param name="type"></param>
		public virtual ISerializable Create(string type)
        {
            ISerializable obj = null;
            foreach (ISerializableFactory inOutableFactory in childFactories)
            {
                obj = inOutableFactory.Create(type);
                if (obj != null)
                {
                    return obj;
                }
            }
            return obj;
        }

        /// <summary>
        /// 
        /// </summary>
        /// <param name="obj"></param>
        /// <returns></returns>
        public virtual string Create(object obj)
        {
            foreach (ISerializableFactory inOutableFactory in childFactories)
            {
                string str = inOutableFactory.Create(obj);
                if (str != null)
                {
                    return str;
                }
            }
            return null;
        }

        /// 
		/// <param name="o"></param>
        public void Remove(ISerializableFactory item)
        {
            childFactories.Remove(item);
        }

	}

}