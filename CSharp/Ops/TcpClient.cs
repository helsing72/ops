///////////////////////////////////////////////////////////
//  TcpClient.cs
//  Implementation of the Class TcpClient
//  Created on:      12-nov-2011 09:25:36
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.Net.Sockets;
using System.IO;

namespace Ops 
{
	public class TcpClient : IReceiver 
    {
        private NetworkStream inputStream; 
		internal Event newBytesEvent = new Event();
		private readonly int receiveBufferSize;
		private readonly string serverIp;
		private readonly int serverPort;
		private byte [] sizePackBytes = new byte[sizePackSize];
		internal static readonly int sizePackSize = 22;
        System.Net.Sockets.TcpClient tcpClient = null;

        public TcpClient(string serverIP, int serverPort, int receiveBufferSize) 
        {
            this.serverPort = serverPort;
            this.serverIp = serverIP;
            this.receiveBufferSize = receiveBufferSize;
            SetupSocket(serverIP, serverPort, receiveBufferSize);
        }

        public void Close()
        {
            throw new NotImplementedException();
        }

        public void Open()
        {
            throw new NotImplementedException();
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
            try
            {
                if (this.tcpClient == null)
                {
                    SetupSocket(serverIp, serverPort, receiveBufferSize);
                }

                // First read the size of the incoming tcp packet
                int accumulatedSize = 0;
                while(accumulatedSize < sizePackSize)
                {
                    accumulatedSize += inputStream.Read(sizePackBytes, accumulatedSize, sizePackSize - accumulatedSize);
                }

                //max_length = *((int*)(data + 18));
                
                //TODO: header and error check.

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
                while(accumulatedSize < headerBytes.Length)
                {
                    accumulatedSize += inputStream.Read(headerBytes, accumulatedSize, headerBytes.Length - accumulatedSize);
                }

                accumulatedSize = 0;
                while(accumulatedSize < dataSize)
                {
                    accumulatedSize += inputStream.Read(bytes, accumulatedSize + offset, dataSize - accumulatedSize);
                }
                
                newBytesEvent.FireEvent((object)(dataSize + headerBytes.Length));
                return true;

            } catch (IOException ex)
            {
                try
                {
                    if (this.tcpClient != null)
                        this.tcpClient.Close();
                }
                catch (IOException)
                {
                    //Already closed?
                    Logger.ExceptionLogger.LogMessage("TCP client close failure: " + ex.ToString());
                }
                finally
                {
                    this.tcpClient = null;
                }
               return false;
            }

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