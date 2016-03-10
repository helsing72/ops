///////////////////////////////////////////////////////////
//  ReceiverFactory.cs
//  Implementation of the Class ReceiverFactory
//  Created on:      12-nov-2011 09:25:35
//  Author:
///////////////////////////////////////////////////////////

using System;
using System.IO;

namespace Ops 
{
	internal class ReceiverFactory 
    {
        public static IReceiver CreateReceiver(Topic topic, String localInterface)
        {
            try
            {
                if (topic.GetTransport().Equals(Topic.TRANSPORT_MC))
                {
                    return new MulticastReceiver(topic.GetDomainAddress(), topic.GetPort(), localInterface, topic.GetInSocketBufferSize());
                }
                else if (topic.GetTransport().Equals(Topic.TRANSPORT_TCP))
                {
                    return new TcpClientReceiver(topic.GetDomainAddress(), topic.GetPort(), topic.GetInSocketBufferSize());
                }
                else if (topic.GetTransport().Equals(Topic.TRANSPORT_UDP))
                {
                    return new UdpReceiver(0, localInterface, topic.GetInSocketBufferSize());
                }
            }
            catch (System.IO.IOException ex)
            {
                Logger.ExceptionLogger.LogMessage("CreateReceiver " + ex.ToString());
            }
            return null;
        }

	}

}