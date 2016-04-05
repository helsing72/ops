///////////////////////////////////////////////////////////
//  TcpClient.cs
//  Implementation of the Class TcpClient
//  Created on:      12-nov-2011 09:25:36
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.IO;
using System.Net.Sockets;
using System.Text;

namespace Ops 
{
	public class TcpClientReceiver : IReceiver
    {
        private NetworkStream inputStream; 
		internal Event newBytesEvent = new Event();
		private readonly int receiveBufferSize;
		private readonly string serverIp;
		private readonly int serverPort;
        private System.Net.Sockets.TcpClient tcpClient = null;
        private bool opened = false;

        // Used for the special tcp header with the fixed length 22 with
        // information about the size of the data package
        private byte[] sizePackBytes = new byte[sizePackSize];
		internal static readonly int sizePackSize = 22;
        private static Encoding encoding = new UTF8Encoding();
        private static byte[] expectedHeader = encoding.GetBytes(Locals.sizeHeader);

        public TcpClientReceiver(string serverIP, int serverPort, int receiveBufferSize) 
        {
            this.serverPort = serverPort;
            this.serverIp = serverIP;
            this.receiveBufferSize = receiveBufferSize;
            Open();
        }

        //TODO Is protection needed here?? See Receive() method
        public void Close()
        {
            if (this.opened && (this.tcpClient != null))
            {
                this.inputStream.Close();
                this.tcpClient.Close();
            }
            this.opened = false;
        }

        public void Open()
        {
            // Open is done by the Receive() method called by the ReceiveDataHandler
            // If we do it here, we will hang for about 1 second if the publisher (i.e server) isn't available
            //if (!this.opened)
            //{
            //    try
            //    {
            //        SetupSocket(this.serverIp, this.serverPort, this.receiveBufferSize);
            //        this.opened = true;
            //    }
            //    catch (SocketException)
            //    {
            //    }
            //}
        }

        /// <summary>
        /// Gets the ip and port of the message sender.
        /// Only valid during the newBytesEvent callback.
        /// </summary>
        /// <param name="IP"></param>
        /// <param name="port"></param>
        public void GetSource(ref string IP, ref int port)
        {
            IP = this.serverIp;
            port = this.serverPort;
        }

        public bool Receive(byte[] headerBytes, byte[] bytes, int offset)
        {
            bool failureDetected = false;
            try
            {
                //TODO Is protection needed here?? See Close() method
                if (this.tcpClient == null)
                {
                    SetupSocket(serverIp, serverPort, receiveBufferSize);
                    this.opened = true;
                }

                // First read the size of the incoming tcp packet
                int accumulatedSize = 0;
                while(!failureDetected && (accumulatedSize < sizePackSize))
                {
                    int numRead = inputStream.Read(sizePackBytes, accumulatedSize, sizePackSize - accumulatedSize);
                    if (numRead == 0)
                    {
                        failureDetected = true; // Other side disconnected
                    }
                    accumulatedSize += numRead;
                }

                if (!failureDetected)
                {
                    // Header check
                    for (int i = 0; i < 18; i++)
                    {
                        if (sizePackBytes[i] != expectedHeader[i])
                        {
                            /// If this header doesn't match we are out of sync, so disconnect
                            throw new Exception("TcpReceiver: Failed to read header !!");
                        }
                    }

                    //max_length = *((int*)(data + 18));

                    int dataSize;
                    if (Globals.BIG_ENDIAN)
                    {
                        byte[] totalSize = new byte[4];
                        Array.Copy(sizePackBytes, 18, totalSize, 0, 4);
                        Array.Reverse(totalSize);
                        dataSize = BitConverter.ToInt32(totalSize, 0) - headerBytes.Length;
                    }
                    else
                    {
                        dataSize = BitConverter.ToInt32(sizePackBytes, 18) - headerBytes.Length;
                    }

                    accumulatedSize = 0;
                    while (!failureDetected && (accumulatedSize < headerBytes.Length))
                    {
                        int numRead = inputStream.Read(headerBytes, accumulatedSize, headerBytes.Length - accumulatedSize);
                        if (numRead == 0)
                        {
                            failureDetected = true; // Other side disconnected
                        }
                        accumulatedSize += numRead;
                    }

                    accumulatedSize = 0;
                    while (!failureDetected && (accumulatedSize < dataSize))
                    {
                        int numRead = inputStream.Read(bytes, accumulatedSize + offset, dataSize - accumulatedSize);
                        if (numRead == 0)
                        {
                            failureDetected = true; // Other side disconnected
                        }
                        accumulatedSize += numRead;
                    }

                    if (!failureDetected)
                    {
                        newBytesEvent.FireEvent((object)(dataSize + headerBytes.Length));
                        return true;
                    }
                }
            } 
            catch (Exception e)
            {
                if (e.GetType() == typeof(SocketException)) {
                    System.Threading.Thread.Sleep(100);
                }
                failureDetected = true;
            }

            if (failureDetected)
            {
                try
                {
                    if (this.tcpClient != null)
                        this.tcpClient.Close();
                }
                catch (IOException ex)
                {
                    //Already closed?
                    Logger.ExceptionLogger.LogMessage("TCP client close failure: " + ex.ToString());
                }
                finally
                {
                    this.tcpClient = null;
                }
            }
            return false;
        }

        public Event GetNewBytesEvent()
        {
            return newBytesEvent;
        }

        private void SetupSocket(string serverIP, int serverPort, int receiveBufferSize) 
        {
            this.tcpClient = new System.Net.Sockets.TcpClient(serverIP, serverPort);
            if (receiveBufferSize > 0) this.tcpClient.ReceiveBufferSize = receiveBufferSize;
            //socket.setSoTimeout(100);
            this.inputStream = tcpClient.GetStream();
        }

	}

}