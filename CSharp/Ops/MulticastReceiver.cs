///////////////////////////////////////////////////////////
//  MulticastReceiver.cs
//  Implementation of the Class MulticastReceiver
//  Created on:      12-nov-2011 09:25:32
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Net;
using System.Net.Sockets;

namespace Ops
{
    public class MulticastReceiver : IReceiver
    {
        private UdpClient multicastSocket;
        private Event newBytesEvent = new Event();
        internal byte[] tempBytes = new byte[Globals.MAX_SEGMENT_SIZE];
        IPEndPoint iep;

        string ip;
        int port;
        string localInterface;
        int receiveBufferSize;
        bool opened;

        public MulticastReceiver(string ip, int port, string localInterface, int receiveBufferSize)
        {
            // Save settings in this instance to be able to Close and Open again, and again,,,
            this.ip = ip;
            this.port = port;
            this.localInterface = localInterface;
            this.receiveBufferSize = receiveBufferSize;

            this.iep = new IPEndPoint(IPAddress.Any, port);

            Open();
        }

        ~MulticastReceiver()
        {
            Close();
        }

        public MulticastReceiver(string ip, int port) : this(ip, port, "0.0.0.0", 0)
        {

        }

        public void Close()
        {
            if (opened && (multicastSocket != null))
            {
                multicastSocket.Close();
            }
            this.opened = false;
        }

        public void Open()
        {
            if (!opened)
            {
                // For multicast receiver sockets it's important to set the "ReuseAddress" socket option
                // before we call bind. Otherwise we will have a conflict with other applications
                // listening on the same port.
                multicastSocket = new UdpClient();
                multicastSocket.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, 1);
                IPEndPoint ep = new IPEndPoint(IPAddress.Any, this.port);
                multicastSocket.Client.Bind(ep);

                if (this.receiveBufferSize > 0)
                {
                    multicastSocket.Client.ReceiveBufferSize = this.receiveBufferSize;
                }

                multicastSocket.Ttl = 1;
                multicastSocket.JoinMulticastGroup(IPAddress.Parse(this.ip), IPAddress.Parse(this.localInterface));

                this.opened = true;
            }
        }

        /// <summary>
        /// Gets the ip and port of the message sender.
        /// Only valid during the newBytesEvent callback.
        /// </summary>
        /// <param name="IP"></param>
        /// <param name="port"></param>
        public void GetSource(ref string IP, ref int port)
        {
            IP = this.iep.Address.ToString();
            port = this.iep.Port;
        }

        public bool Receive(byte[] headerBytes, byte[] bytes, int offset)
        {
            try
            {
                byte[] data = multicastSocket.Receive(ref iep);

                Buffer.BlockCopy(data, 0, headerBytes, 0, headerBytes.Length);
                Buffer.BlockCopy(data, headerBytes.Length, bytes, offset, data.Length - headerBytes.Length);

                newBytesEvent.FireEvent((int)(data.Length));
                return true;
            }
            catch (Exception)
            {
                return false;
            }
        }

        public Event GetNewBytesEvent()
        {
            return newBytesEvent;
        }

    }

}