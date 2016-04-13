#ReceiveDataHandler

import socket
import thread

import Support
from Constants import *
import DataAssembly

class AbstractReceiveDataHandler(object):
	def __init__(self,localInterface,topic):
		super(AbstractReceiveDataHandler,self).__init__()
		self.topic = topic
		self.localInterface = localInterface
		self.assembler = None
		self.subscrubers = set()

	def segmentReceived(self,data,addr):
		segment = DataAssembly.Segment(data)
		if segment.isValid():
			if (self.assembler==None):
				self.assembler = DataAssembly.Assembler()
	
			if (self.assembler.addSegment(segment)==False):
				self.assembler = DataAssembly.Assembler()	
			else:
				if (self.assembler.isFull()):
					obj = self.assembler.createOPS(self.topic.participant.objectFactory)
					obj.setSource(addr)
					self.distributeMessage(obj)
					self.assembler = None

	def addSubscriber(self,subs):
		self.subscrubers.add(subs)
	def removeSubscriber(self,subs):
		self.subscrubers.remove(subs)

	def distributeMessage(self,obj):
		if obj is not None:
			for subs in self.subscrubers:
				subs.newMessage(obj)

	def run(self):
		raise NotImplementedError
	def start(self):
		self.shouldRun = True
		thread.start_new_thread( self.run, () )
	def end(self):
		self.shouldRun = False
		pass

class TcpReceiveDataHandler(AbstractReceiveDataHandler):
	def __init__(self):
		super(TcpReceiveDataHandler,self).__init__()
		raise NotImplementedError

class UdpReceiveDataHandler(AbstractReceiveDataHandler):
	def __init__(self,localInterface,topic):
		super(UdpReceiveDataHandler,self).__init__(localInterface,topic)

class McReceiveDataHandler(AbstractReceiveDataHandler):
	def __init__(self,localInterface,topic):
		super(McReceiveDataHandler,self).__init__(localInterface,topic)

	def run(self):
		sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM, socket.IPPROTO_UDP)
		sock.setsockopt(socket.SOL_SOCKET,socket.SO_REUSEADDR,1)
		sock.bind(('',self.topic.port))

		group = socket.inet_aton(self.topic.domainAddress)
		iface = socket.inet_aton(self.localInterface)

		sock.setsockopt(socket.IPPROTO_IP,socket.IP_ADD_MEMBERSHIP,group + iface)
  		if self.topic.inSocketBufferSize > 0:
  			sock.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, self.topic.inSocketBufferSize)

		while self.shouldRun:
			try:
				data, addr = sock.recvfrom(PACKET_MAX_SIZE);
				self.segmentReceived(data,addr)
			except socket.timeout:
				pass

		sock.close()

def __makeKey(topic):
	return topic.transport + "::" + topic.domainAddress + "::" + str(topic.port)

__ReceiveDataHandler = {}


def getReceiveDataHandler(participant,topic):
	key = __makeKey(topic)

	rdh = None
	if key in __ReceiveDataHandler:
		rdh = __ReceiveDataHandler[key]
		if max(rdh.topic.sampleMaxSize,topic.sampleMaxSize) > ops.PACKET_MAX_SIZE:
			message = "Warning: "
			if topic.transport == ops.Topic.TRANSPORT_UDP:
				message += "UDP Transport"
			else:
				message += "Same port (" +  topic.port + ")"
			message += "is used with Topics with 'sampleMaxSize' > " + ops.PACKET_MAX_SIZE
			print message
	else:
		localInterface = Support.doSubnetTranslation(participant.domain.localInterface)
		if topic.transport == TRANSPORT_MC:	
			rdh = McReceiveDataHandler(localInterface,topic)
		rdh.start()
		__ReceiveDataHandler[key] = rdh
	return rdh