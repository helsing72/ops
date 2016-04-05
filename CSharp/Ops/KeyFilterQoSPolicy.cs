///////////////////////////////////////////////////////////
//  KeyFilterQoSPolicy.cs
//  Implementation of the Class KeyFilterQoSPolicy
//  Created on:      12-nov-2011 09:25:31
//  Author:
///////////////////////////////////////////////////////////

using System.Runtime.CompilerServices;  
using System.Collections.Generic;       

namespace Ops 
{
	public class KeyFilterQoSPolicy : IFilterQoSPolicy 
    {
		private List<string> keys;
        private System.Object lockThis = new System.Object();

        public KeyFilterQoSPolicy()
        {
            this.SetKeys(new List<string>());
            
        }
 
        public KeyFilterQoSPolicy(string key)
        {
            this.SetKeys(new List<string>());
            this.GetKeys().Add(key);
        }

        public KeyFilterQoSPolicy(List<string> keys)
        {
            this.SetKeys(keys);
        }
        
        public List<string> GetKeys()
        {
            return keys;
        }
        
        [MethodImpl(MethodImplOptions.Synchronized)]
        public void SetKeys(List<string> keys)
        {
            this.keys = keys;
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public void SetKey(string key)
        {
            this.keys.Clear();
            this.keys.Add(key);
        }
        
        public bool ApplyFilter(OPSObject o)
        {
            lock(lockThis)
            {
                foreach (string key in keys)
                {
                    if (o.GetKey().Equals(key))
                    {
                        return true;
                    }
                }
                return false;
            }
        }


	}

}