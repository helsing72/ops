///////////////////////////////////////////////////////////
//  TcpSenderList.cs
//  Implementation of the Class TcpSenderList
//  Created on:      12-nov-2011 09:25:36
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Collections.Generic;       // Needed for the "List"
using System.IO;
using System.Net;
using System.Net.Sockets;
using System.Runtime.CompilerServices;  // Needed for the "MethodImpl" synchronization attribute
using System.Text;

namespace Ops 
{
	public class TcpSenderList : ISender 
    {
        internal List<System.Net.Sockets.TcpClient> senders = new List<System.Net.Sockets.TcpClient>();
        private Encoding encoding = new UTF8Encoding();

        public void Open()
        {
            // Nothing to do
        }

        public void Close()
        {
            // Nothing to do
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public void EmptyList()
        {
            foreach (System.Net.Sockets.TcpClient s in senders)
            {
                s.Close();
            }
            senders.Clear();
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public bool Remove(System.Net.Sockets.TcpClient o)
        {
            return senders.Remove(o);
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public void Add(System.Net.Sockets.TcpClient e)
        {
            senders.Add(e);
        }

        /// <summary>
        /// Get IP and port used as endpoint when sending a message
        /// </summary>
        /// <param name="IP"></param>
        /// <param name="port"></param>
        public void GetLocalEndpoint(ref string IP, ref int port)
        {
            ///Not used
            IP = "";
            port = 0;
        }

        // Synchronization is unnecessary since the code only calls a synchronized method
        public bool SendTo(byte[] bytes, string ip, int port)
        {
            return this.SendTo(bytes, 0, bytes.Length, ip, port);
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public bool SendTo(byte[] bytes, int offset, int size, string ip, int port)
        {
            List<System.Net.Sockets.TcpClient> failedSenders = new List<System.Net.Sockets.TcpClient>();

            foreach (System.Net.Sockets.TcpClient s in senders)
            {
                ///IPEndPoint endPoint = (IPEndPoint)s.Client.RemoteEndPoint;
                try
                {
                    // First, prepare and send a package of fixed length 22 with 
                    // information about the size of the data package
                    byte[] str = encoding.GetBytes(Locals.sizeHeader);
                    s.GetStream().Write(str, 0, str.Length);

                    byte[] sizebuf = BitConverter.GetBytes(size);
                    if (Globals.BIG_ENDIAN) Array.Reverse(sizebuf);
                    s.GetStream().Write(sizebuf, 0, sizebuf.Length);

                    //Send the actual data
                    s.GetStream().Write(bytes, offset, size);
                } 
                catch (IOException)
                {
                    ///TODO Add event handling for connect/disconnect of subscribers
                    
                    // Remove directly on 'senders' is not allowed here since it changes the list we loop over
                    // Save in separate list and remove later.
                    failedSenders.Add(s);
                }
                    
            }

            foreach (System.Net.Sockets.TcpClient s in failedSenders)
            {
                Remove(s);
            }

            return true;
        }

        // Synchronization is unnecessary since the code only calls a synchronized method
        public bool SendTo(byte[] bytes, int offset, int size, InetAddress ipAddress, int port)
        {
            return this.SendTo(bytes, offset, size, ipAddress.GetHostAddress(), port);
        }

	}

}