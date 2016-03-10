///////////////////////////////////////////////////////////
//  Receiver.cs
//  Implementation of the Interface Receiver
//  Created on:      12-nov-2011 09:25:35
//  Author:
///////////////////////////////////////////////////////////

namespace Ops
{
    public interface IReceiver
    {
        void Close();

        void Open();

        bool Receive(byte[] headerBytes, byte[] bytes, int offset);

        Event GetNewBytesEvent();

        void GetSource(ref string IP, ref int port);
    }

}