/**
* 
* Copyright (C) 2006-2009 Anton Gravestam.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.

* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*/

#ifndef ops_UDPSenderH
#define	ops_UDPSenderH

#include "Sender.h"
#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include "Notifier.h"

namespace ops
{
    ///A sender implementation that dispatches messages over ip based UDP.
    class UDPSender : public Sender
    {
    public:
        ///Costructs a new UDPSender an binds its underlying socket to local host
        ///and a dynamically allocated local port.
		///This class accepts synchronous write operations through sendTo().

        UDPSender(std::string localInterface = "0.0.0.0", int ttl = 1, __int64 outSocketBufferSize = 16000000, bool multicastSocket = false);
        ~UDPSender();
        
		void open();
		void close();

        ///Override from Sender
        bool sendTo(char* buf, int size, const std::string& ip, int port);
		///Override from Sender
		///int receiveReply(char* buf, int size);
        ///Override from Sender
        int getPort() {return socket->local_endpoint().port();};
        ///Override from Sender
        std::string getAddress() {return socket->local_endpoint().address().to_string();};

		///bool waitForReply(int timeout);

    private:
        ///This UDPSender wraps boost socket functionality.
        ///These are the required boost members to perform operations required.
        boost::asio::ip::udp::endpoint* localEndpoint;  //<-- The local port to bind to.
        boost::asio::ip::udp::socket* socket;           //<-- The socket that sends data.
        boost::asio::io_service io_service;             //<-- Required for boost sockets.

		std::string localInterface;
		int ttl;
		__int64 outSocketBufferSize;
		bool multicastSocket;

    };

}
#endif	
