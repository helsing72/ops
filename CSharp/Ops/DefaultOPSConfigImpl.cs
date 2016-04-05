///////////////////////////////////////////////////////////
//  DefaultOPSConfigImpl.cs
//  Implementation of the Class DefaultOPSConfigImpl
//  Created on:      12-nov-2011 09:25:29
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public class DefaultOPSConfigImpl : OPSConfig 
    {
        public DefaultOPSConfigImpl()
        {
            AppendType("DefaultOPSConfigImpl");
        }

        public override void Serialize(IArchiverInOut archive) 
        {
            base.Serialize(archive);
        }
	}

}