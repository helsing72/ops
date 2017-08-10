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

#include "NetworkSupport.h"

#ifndef REPLACE_TRANSPORT_LAYER

#ifdef __GNUC__
#pragma GCC diagnostic ignored "-Wunused-variable"
#endif

#include "BoostIOServiceImpl.h"

namespace ops
{

// If argument contains a "/" we assume it is on the form:  subnet-address/subnet-mask
// e.g "192.168.10.0/255.255.255.0" or "192.168.10.0/24"
// In that case we loop over all interfaces and take the first one that matches
// i.e. the one whos interface address is on the subnet
Address_T doSubnetTranslation(Address_T addr, IOService* ioServ)
{
	using boost::asio::ip::udp;

	Address_T::size_type index;

	index = addr.find("/");
	if (index == Address_T::npos) return addr;

	Address_T subnet = addr.substr(0, index);
	Address_T mask = addr.substr(index+1);

	unsigned long subnetIp = boost::asio::ip::address_v4::from_string(subnet.c_str()).to_ulong();
	unsigned long subnetMask;
	if (mask.length() <= 2) {
		// Expand to the number of bits given
		subnetMask = atoi(mask.c_str());
		subnetMask = (((1 << subnetMask)-1) << (32 - subnetMask)) & 0xFFFFFFFF;
	} else {
		subnetMask = boost::asio::ip::address_v4::from_string(mask.c_str()).to_ulong();
	}

	boost::asio::io_service* ioService = dynamic_cast<BoostIOServiceImpl*>(ioServ)->boostIOService;

	// Note: The resolver requires that the hostname can be used to resolve to an ip
	// e.g due to the hostname beeing listed with an ipv4 address in /etc/hosts.
	// On linux this can be tested by using the command "hostname -i"
	udp::resolver resolver(*ioService);
	udp::resolver::query query(boost::asio::ip::host_name(), "");
	udp::resolver::iterator it = resolver.resolve(query);
	udp::resolver::iterator end;
	while (it != end) {
		boost::asio::ip::address ipaddr = it->endpoint().address();
		if (ipaddr.is_v4()) {
			unsigned long Ip = ipaddr.to_v4().to_ulong();
			if ((Ip & subnetMask) == (subnetIp & subnetMask)) 
				return ipaddr.to_string().c_str();
		}
		++it;
	}

	return subnet;
}

InternalString_T GetHostName()
{
	char hname[1024];
	hname[0] = '\0';
	gethostname(hname, sizeof(hname));
	return hname;
}

}
#endif // REPLACE_TRANSPORT_LAYER
