///////////////////////////////////////////////////////////
//  SerializableFactory.cs
//  Implementation of the Interface SerializableFactory
//  Created on:      12-nov-2011 21:02:40
//  Author:
///////////////////////////////////////////////////////////

namespace Ops
{
	public interface ISerializableFactory 
    {
        ISerializable Create(string type);

        string Create(object obj);
    }

}