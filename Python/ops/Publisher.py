from Participant import Participant
from opsTypes import Message

from OPS_Archiver import OPS_Archiver_Out
from DataAssembly import Segment
from Constants import *

class Publisher(object):
	@staticmethod
	def chunkstring(string, length):
		return [string[0+i:length+i] for i in range(0, len(string), length)]

	def __init__(self,topic):
		super(Publisher, self).__init__()
		self.topic = topic
		self.key=""
		self.name=""

		self.message = Message()
		self.message.publisherName = self.name
		self.message.topicName = topic.name

		self.currentPublicationID=0
		self.participant = Participant.getInstance(topic.domainID, topic.participantID)
		self.sendDataHandler = self.participant.getSendDataHandler(topic)

	def write(self,data):
		if self.key != "":
			data.key=self.key
		self.message.data = data
		self.message.publisherName = self.name
		self.message.publicationID = self.currentPublicationID;

		archiver = OPS_Archiver_Out(self.topic.sampleMaxSize)
		archiver.Ops("message",self.message)
		archiver.Spare(data.spareBytes)

		blocks = Publisher.chunkstring(archiver.buffer,PACKET_MAX_SIZE - SEGMENTSIZE)
		for i in range(len(blocks)):
			segment = Segment()
			segment.NumberOfSegments = len(blocks)
			segment.currentSegment = i
			block = segment.serialize() + blocks[i]
			self.sendDataHandler.sendData(block,self.topic)
		self.currentPublicationID+=1

	def start(self):
		self.sendDataHandler.addPublisher(self)

	def stop(self):
		self.sendDataHandler.removePublisher(self)

	def getTopic(self):
		return self.topic

