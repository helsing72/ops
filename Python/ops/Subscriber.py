from opsTypes import Message
from Participant import Participant
from OPS_Archiver import OPS_Archiver_In
from Constants import *

class Subscriber(object):
	def __init__(self,topic):
		super(Subscriber,self).__init__()
		self.topic = topic
		self.participant = Participant.getInstance(topic.domainID,topic.participantID)
		self.receiveDataHandler=None

		self.filters = []
		self.listeners = set()
		self.messageCallbacks = set()
		self.started = False

	def __del__(self):
		self.disconnect()	

	def start(self):
		if self.started:
			return
		self.receiveDataHandler = self.participant.getReceiveDataHandler(self.topic)
		self.receiveDataHandler.addSubscriber(self)
		self.started = True
	
	def stop(self):
		if not self.started:
			return
		self.receiveDataHandler.removeListener(self);
		self.receiveDataHandler = None
		self.started = False

	def getTopic(self):
		return self.topic

	def applyFilters(self,obj):
		for f in self.filters:
			if not f.applyFilter(obj):
				return False
		return True


	def disconnect(self):
		if self.rdh is not None:
			self.rdh.removeSubscriber(self)
		self.rdh=None

	def newMessage(self,message):
		if message.topicName == self.topic.name:
			if self.topic.typeID in message.data.typesString:
				if self.applyFilters(message.data):
# switch to callback
#					for l in self.listeners:
#						l.onNewData(message)
					for c in self.messageCallbacks:
						c(self,message)

# switch to callback
#	def addListener(self,listener):
#		self.listeners.add(listener)
#	def removeListener(self,listener):
#		self.listeners.remove(listener)


	def addCallback(self,callback):
		self.messageCallbacks.add(callback)
	def removeCallback(self,callback):
		self.messageCallbacks.remove(callback)

