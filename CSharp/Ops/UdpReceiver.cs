///////////////////////////////////////////////////////////
//  UdpReceiver.cs
//  Implementation of the Class UdpReceiver
//  Created on:      04-jan-2013
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Net;
using System.Net.Sockets;

namespace Ops
{
    public class UdpReceiver : IReceiver
    {
        private UdpClient udpSocket;
        private Event newBytesEvent = new Event();
        internal byte[] tempBytes = new byte[Globals.MAX_SEGMENT_SIZE];
        IPEndPoint iep;

        public string IP { get; private set; }
        public int Port { get; private set; }

        int port;
        string localInterface;
        int receiveBufferSize;
        bool opened = false;

        public UdpReceiver(int port, string localInterface, int receiveBufferSize)
        {
            // Save settings in this instance to be able to Close and Open again, and again,,,
            this.port = port;
            this.localInterface = localInterface;
            this.receiveBufferSize = receiveBufferSize;

            this.iep = new IPEndPoint(IPAddress.Any, port);

            Open();
        }

        ~UdpReceiver()
        {
            Close();
        }

        public UdpReceiver(int port) : this(port, "0.0.0.0", 0)
        {

        }

        public void Close()
        {
            if (opened && (udpSocket != null))
            {
                udpSocket.Close();
            }
            this.opened = false;
        }

        public void Open()
        {
            if (!opened)
            {
                //// For udp receiver sockets it's important to set the "ReuseAddress" socket option
                //// before we call bind. Otherwise we will have a conflict with other applications
                //// listening on the same port.
                udpSocket = new UdpClient();
                //udpSocket.Client.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, 1);

                string IpAddress = "0.0.0.0";
                // We need to bind to a specific interface so we can get an IP address to publish to udp senders
                if (this.localInterface.Equals("0.0.0.0"))
                {
                    System.Net.NetworkInformation.IPGlobalProperties gp = System.Net.NetworkInformation.IPGlobalProperties.GetIPGlobalProperties();
                    System.Net.NetworkInformation.UnicastIPAddressInformationCollection x = gp.GetUnicastAddresses();
                    for (int i = 0; i < x.Count; i++)
                    {
                        if (x[i].Address.AddressFamily == AddressFamily.InterNetwork) //IPV4
                        {
                            IpAddress = x[i].Address.ToString();
                            break;
                        }
                    }
                }
                else
                {
                    IpAddress = this.localInterface;
                }

                // If this.port == 0, the system will assign us a port. We fetch the used port below.
                IPEndPoint ep = new IPEndPoint(IPAddress.Parse(IpAddress), this.port);
                udpSocket.Client.Bind(ep);

                // Fetch actualy used ip and port that we listen to
                this.IP = ((IPEndPoint)udpSocket.Client.LocalEndPoint).Address.ToString();
                this.Port = ((IPEndPoint)udpSocket.Client.LocalEndPoint).Port;

                if (this.receiveBufferSize > 0)
                {
                    udpSocket.Client.ReceiveBufferSize = this.receiveBufferSize;
                }

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
                byte[] data = udpSocket.Receive(ref iep);

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