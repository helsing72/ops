/**
* 
* Copyright (C) 2006-2009 Anton Gravestam.
* Copyright (C) 2019 Lennart Andersson.
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

#include <iostream>

#include "OPSTypeDefs.h"

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif

#include "boost/asio/basic_datagram_socket.hpp"

#include "UDPSender.h"
#include "Participant.h"
#include "BasicError.h"
#include "BasicWarning.h"
#include "BoostIOServiceImpl.h"

namespace ops
{
    using boost::asio::ip::udp;
	UDPSender::UDPSender(IOService* ioServ, Address_T localInterface, int ttl, int64_t outSocketBufferSize, bool multicastSocket):
		ipAddr(boost::asio::ip::address_v4::from_string(localInterface.c_str())),
		localEndpoint(ipAddr, 0),
		socket(nullptr), io_service(dynamic_cast<BoostIOServiceImpl*>(ioServ)->boostIOService),
		_localInterface(localInterface), _ttl(ttl), _outSocketBufferSize(outSocketBufferSize), _multicastSocket(multicastSocket)
    {
		open();
    }

    UDPSender::~UDPSender()
    {
		close();
    }

	void UDPSender::open()
	{
		if (socket != nullptr) return;

        socket = new boost::asio::ip::udp::socket(*io_service, localEndpoint.protocol());
		try {
			if(_outSocketBufferSize > 0)
			{
				boost::asio::socket_base::send_buffer_size option((int)_outSocketBufferSize);
				boost::system::error_code ec;
				ec = socket->set_option(option, ec);
				socket->get_option(option);
				if(ec || option.value() != _outSocketBufferSize)
				{
					ErrorMessage_T msg("Socket buffer size ");
					msg += NumberToString(_outSocketBufferSize);
					msg += " could not be set. Used value: ";
					msg += NumberToString(option.value());
					ops::BasicWarning err("UDPSender", "UDPSender", msg);
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

#if BOOST_VERSION > 106500
			socket->non_blocking(true);
#else
			boost::asio::socket_base::non_blocking_io command(true);
			socket->io_control(command);
#endif

			socket->bind(localEndpoint);
		}
		catch (...) {
			socket->close();
			delete socket;
			socket = nullptr;
			throw;
		}
	}

	void UDPSender::close()
	{
		if (socket != nullptr) {
	        socket->close();
			delete socket;
			socket = nullptr;
		}
	}

    bool UDPSender::sendTo(const char* buf, const int size, const Address_T& ip, const int port)
    {
		if (socket == nullptr) { return false; }
        try
        {
            boost::asio::ip::address ipaddress = boost::asio::ip::address::from_string(ip.c_str());
            boost::asio::ip::udp::endpoint endpoint(ipaddress, port);
            std::size_t res = socket->send_to(boost::asio::buffer(buf, size), endpoint);
			if (res != (std::size_t)size) {
				OPS_UDP_ERROR("UDPSender: sendTo(), Error: Failed to write message (" << size << "), res: " << res << "]\n");
				return false;
			}
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

