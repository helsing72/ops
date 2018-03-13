import struct
import socket

def getInterfaceList():
	return socket.gethostbyname_ex(socket.gethostname())[2] + socket.gethostbyname_ex('localhost')[2]


def doSubnetTranslation(localInterface):
	if "/" not in localInterface:
		return localInterface
	networkAddress,netMask = localInterface.split("/")
	if len(netMask)<3:
		netMask = (0xffffffff << (32 - int(netMask))) & 0xffffffff
	else:
		netMask = struct.unpack(">I",socket.inet_aton(netMask))[0]
	networkAddress = struct.unpack(">I",socket.inet_aton(networkAddress))[0]

	for iface in getInterfaceList():
		temp = struct.unpack(">I",socket.inet_aton(iface))[0]
		if ((temp ^ networkAddress) & netMask) == 0:
			return iface
