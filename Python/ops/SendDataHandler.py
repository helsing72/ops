import socket
import Support
from Constants import *

class AbstractSendDataHandler(object):
	def __init__(self):
		super(AbstractSendDataHandler,self).__init__()
		self.publishers = set()

	def addPublisher(self,client):
		self.publishers.add(client)
		if len(self.publishers)==1:
			self.open()

	def removePublisher(self,client):
		self.publishers.remove(client)
		if len(self.publishers)==0:
			self.close()

	def open(self):
		pass

	def close(self):
		pass

	def sendData(self,block,topic):
		raise NotImplementedError


class UdpSendDataHandler(AbstractSendDataHandler):
	def __init__(self,topic):
		super(UdpSendDataHandler,self).__init__()
		self.sendAddress = (topic.domainAddress, topic.port)
  		self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
  		if topic.outSocketBufferSize > 0:
  			self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, topic.outSocketBufferSize)

	def sendData(self,block,topic):
		if self.socket is None:
			return
		self.socket.sendto(block,self.sendAddress)


class TcpSendDataHandler(AbstractSendDataHandler):
	def __init__(self):
		super(TcpSendDataHandler,self).__init__()
		pass
	def sendData(self,block,topic):
		raise NotImplementedError


class McSendDataHandler(AbstractSendDataHandler):
	def __init__(self,localInterface,topic,ttl):
		super(McSendDataHandler,self).__init__()
		self.sendAddress = (topic.domainAddress, topic.port)
  		self.socket = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
  		self.socket.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_TTL, ttl)
  		self.socket.setsockopt(socket.IPPROTO_IP, socket.IP_MULTICAST_IF, socket.inet_aton(localInterface))
  		if topic.outSocketBufferSize > 0:
  			self.socket.setsockopt(socket.SOL_SOCKET, socket.SO_SNDBUF, topic.outSocketBufferSize)

	def sendData(self,block,topic):
		if self.socket is None:
			return
		self.socket.sendto(block,self.sendAddress)

def getSendDataHandler(participant,topic):
	localInterface = Support.doSubnetTranslation(topic.localInterface)

	if topic.transport == TRANSPORT_MC:
		return McSendDataHandler(localInterface,topic,topic.timeToLive)
	if topic.transport == TRANSPORT_UDP:
		return UdpSendDataHandler(topic)

	return None
