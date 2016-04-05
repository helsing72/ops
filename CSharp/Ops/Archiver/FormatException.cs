///////////////////////////////////////////////////////////
//  FormatException.cs
//  Implementation of the Class FormatException
//  Created on:      12-nov-2011 21:02:40
//  Author:
///////////////////////////////////////////////////////////

using System;

namespace Ops 
{
    [Serializable()]
    public class FormatException : System.Exception
    {
        public FormatException() : base() { }
        public FormatException(string message) : base(message) { }
        public FormatException(string message, System.Exception inner) : base(message, inner) { }

        // A constructor is needed for serialization when an
        // exception propagates from a remoting server to the client. 
        protected FormatException(System.Runtime.Serialization.SerializationInfo info,
            System.Runtime.Serialization.StreamingContext context) { }
    }

}