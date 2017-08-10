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

//#include "boost\asio\basic_socket.hpp"

#include "OPSTypeDefs.h"

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif

#include "boost/asio/basic_datagram_socket.hpp"

#include "UDPSender.h"
//#include "TimeHelper.h"
#include <iostream>
#include "Participant.h"
#include "BasicError.h"
#include "BoostIOServiceImpl.h"

namespace ops
{
    using boost::asio::ip::udp;
	UDPSender::UDPSender(IOService* ioServ, Address_T localInterface, int ttl, __int64 outSocketBufferSize, bool multicastSocket):
		socket(NULL), io_service(dynamic_cast<BoostIOServiceImpl*>(ioServ)->boostIOService),
		_localInterface(localInterface), _ttl(ttl), _outSocketBufferSize(outSocketBufferSize), _multicastSocket(multicastSocket)
    {
		boost::asio::ip::address ipAddr(boost::asio::ip::address_v4::from_string(localInterface.c_str()));

		localEndpoint = new boost::asio::ip::udp::endpoint(ipAddr, 0);

		open();
    }

    UDPSender::~UDPSender()
    {
		close();
        delete localEndpoint;
    }

	void UDPSender::open()
	{
		if (socket != NULL) return;

        socket = new boost::asio::ip::udp::socket(*io_service, localEndpoint->protocol());

		if(_outSocketBufferSize > 0)
		{
			boost::asio::socket_base::send_buffer_size option((int)_outSocketBufferSize);
			boost::system::error_code ec;
			ec = socket->set_option(option, ec);
			socket->get_option(option);
			if(ec != 0 || option.value() != _outSocketBufferSize)
			{
				//std::cout << "Socket buffer size could not be set" << std::endl;
				ops::BasicError err("UDPSender", "UDPSender", "Socket buffer size could not be set");
				Participant::reportStaticError(&err);
			}
		}

		if(_multicastSocket)
		{
			boost::asio::ip::multicast::hops ttlOption(_ttl);
			socket->set_option(ttlOption);

			boost::asio::ip::address_v4 local_interface = boost::asio::ip::address_v4::from_string(_localInterface.c_str());
			boost::asio::ip::multicast::outbound_interface ifOption(local_interface);
			socket->set_option(ifOption);
		}

		boost::asio::socket_base::non_blocking_io command(true);
		socket->io_control(command);
	}

	void UDPSender::close()
	{
		if (socket) {
	        socket->close();
			delete socket;
			socket = NULL;
		}
	}

    bool UDPSender::sendTo(char* buf, int size, const Address_T& ip, int port)
    {
		if (!socket) return false;
        try
        {
            boost::asio::ip::address ipaddress = boost::asio::ip::address::from_string(ip.c_str());
            boost::asio::ip::udp::endpoint endpoint(ipaddress, port);
            socket->send_to(boost::asio::buffer(buf, size), endpoint);
            return true;
        }
		catch (std::exception& ex)
        {
			ErrorMessage_T msg("Error when sending udp message: ");
			msg += ex.what();
			msg += " Params: buf = ";
			msg += NumberToString((size_t)buf);
			msg += ", size = ";
			msg += NumberToString(size);
			msg += ", ip = ";
			msg += ip;
			msg += ", port = ";
			msg += NumberToString(port);
			ops::BasicError err("UDPSender", "sendTo", msg);
			Participant::reportStaticError(&err);
            return false;
        }
        catch (...)
        {
			ErrorMessage_T msg("Error when sending udp message: Params: buf = ");
			msg += NumberToString((size_t)buf);
			msg += ", size = ";
			msg += NumberToString(size);
			msg += ", ip = ";
			msg += ip;
			msg += ", port = ";
			msg += NumberToString(port);
			ops::BasicError err("UDPSender", "sendTo", msg);
			Participant::reportStaticError(&err);
            return false;
        }

    }

}

