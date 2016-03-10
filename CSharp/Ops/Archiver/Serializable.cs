///////////////////////////////////////////////////////////
//  Serializable.cs
//  Implementation of the Interface Serializable
//  Created on:      12-nov-2011 21:02:40
//  Author:
///////////////////////////////////////////////////////////

namespace Ops
{
	public interface ISerializable 
    {
		void Serialize(IArchiverInOut archive);
	}

}