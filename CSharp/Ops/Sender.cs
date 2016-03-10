///////////////////////////////////////////////////////////
//  Sender.cs
//  Implementation of the Interface Sender
//  Created on:      12-nov-2011 09:25:36
//  Author:
///////////////////////////////////////////////////////////

namespace Ops 
{
	public interface ISender 
    {
        /// <summary>
        /// 
        /// </summary>
        void Open();
        /// <summary>
        /// 
        /// </summary>
        void Close();
		/// 
		/// <param name="bytes"></param>
		/// <param name="ip"></param>
		/// <param name="port"></param>
		bool SendTo(byte[] bytes, string ip, int port);

		/// 
		/// <param name="bytes"></param>
		/// <param name="offset"></param>
		/// <param name="size"></param>
		/// <param name="ip"></param>
		/// <param name="port"></param>
		bool SendTo(byte[] bytes, int offset, int size, string ip, int port);

		/// 
		/// <param name="bytes"></param>
		/// <param name="offset"></param>
		/// <param name="size"></param>
		/// <param name="ipAddress"></param>
		/// <param name="port"></param>
		bool SendTo(byte[] bytes, int offset, int size, InetAddress ipAddress, int port);

        /// <summary>
        /// Get IP and port used as endpoint when sending a message
        /// </summary>
        /// <param name="IP"></param>
        /// <param name="port"></param>
        void GetLocalEndpoint(ref string IP, ref int port);
	}

}