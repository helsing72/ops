# Participant

from socket import gethostname
from os import getpid

try:
	import thread
except ImportError:
	import _thread as thread	# module renamed in Python3

from time import sleep

from ops.opsTypes import Config,Topic
from ops.Factory import OpsDefaultFactory
import ops.SendDataHandler
import ops.ReceiveDataHandler
import ops.ParticipantInfoData
import ops.Subscriber
import ops.Publisher

class Participant(object):
	instances = {}
	@staticmethod
	def __createName(domainID, participantID):
		return domainID+"_"+participantID

	@staticmethod
	def getInstance(domainID, participantID="DEFAULT_PARTICIPANT", configFile=None):
		name = Participant.__createName(domainID, participantID)
		if not name in Participant.instances:
			part = Participant(domainID, participantID, configFile)
			if part.domain is None:
				return None
			Participant.instances[name] = part
		return Participant.instances[name]

	def __init__(self,domainID, participantID, configFile):
		super(Participant, self).__init__()
		self.domainID = domainID
		self.participantID = participantID

		if configFile is None:
			self.config = Config.getConfig()
		else:
			self.config = Config.getConfig(configFile)
		self.domain = self.config.getDomain(domainID)
		self.objectFactory = OpsDefaultFactory()

		self.shouldRun = False

		self.partInfoSub = None
		self.partInfoPub = None
		self.partInfoData = ops.ParticipantInfoData.ParticipantInfoData()

		self.partInfoData.name = gethostname() + "(" + str(getpid()) + ")"
		self.partInfoData.languageImplementation = "Python"
		self.partInfoData.id = participantID
		self.partInfoData.domain = domainID

	def __str__(self):
		return "Participant\nConfig:\n" + self.config.__str__() + "\nDomain:\n" + self.domain.__str__()

	def createTopic(self,name):
		topic = self.domain.getTopic(name)
		topic.participantID = self.participantID;
		topic.domainID = self.domainID;
		topic.participant = self;
		return topic;

	def addTypeSupport(self,typeSupport):
		self.objectFactory.addFactory(typeSupport)

	def getSendDataHandler(self,topic):
		self.start()
		self.partInfoData.publishTopics.append(ops.ParticipantInfoData.TopicInfoData(topic))
		return ops.SendDataHandler.getSendDataHandler(self,topic)
	def releaseSendDataHandler(self,topic):
		for i, ti in enumerate(self.partInfoData.publishTopics):
			if ti.name == topic.name:
				del self.partInfoData.publishTopics[i]
				break
		ops.SendDataHandler.releaseSendDataHandler(self,topic)

	def getReceiveDataHandler(self,topic):
		self.start()
		self.partInfoData.subscribeTopics.append(ops.ParticipantInfoData.TopicInfoData(topic))
		return ops.ReceiveDataHandler.getReceiveDataHandler(self,topic)
	def releaseReceiveDataHandler(self,topic):
		for i, ti in enumerate(self.partInfoData.subscribeTopics):
			if ti.name == topic.name:
				del self.partInfoData.subscribeTopics[i]
				break
		ops.ReceiveDataHandler.releaseReceiveDataHandler(self,topic)

	def createParticipantInfoTopic(self):
		topic = Topic()
		topic.domainAddress = self.domain.domainAddress
		topic.localInterface = self.domain.localInterface;
		topic.timeToLive = self.domain.timeToLive;
		topic.participantID = self.participantID;
		topic.domainID = self.domainID;
		topic.participant = self;
		topic.port = self.domain.metaDataMcPort
		topic.name = "ops.bit.ParticipantInfoTopic"
		topic.typeID = "ops.ParticipantInfoData"
		return topic

	def createDebugTopic(self):
		topic = Topic()
		topic.domainAddress = self.domain.domainAddress
		topic.localInterface = self.domain.localInterface;
		topic.timeToLive = self.domain.timeToLive;
		topic.participantID = self.participantID;
		topic.domainID = self.domainID;
		topic.participant = self;
		topic.port = self.domain.debugMcPort
		topic.name = "ops.DebugTopic"
		topic.typeID = "opsidls.DebugRequestResponseData"
		return topic

	def run(self):
		while self.shouldRun:
			sleep(1)
			# Create subscriber
			if self.partInfoSub is None and self.domain.metaDataMcPort > 0:
				self.partInfoSub = ops.Subscriber.Subscriber(self.createParticipantInfoTopic())
				self.partInfoSub.addCallback(self.onParticipantInfoData)
				self.partInfoSub.start()
			# create publisher
			if self.partInfoPub is None and self.domain.metaDataMcPort > 0:
				self.partInfoPub = ops.Publisher.Publisher(self.createParticipantInfoTopic())

			# periodic publish of our ParticipantInfoData
			if self.partInfoPub is not None:
				self.partInfoPub.write(self.partInfoData)

		if self.partInfoSub is not None:
			self.partInfoSub.stop()
			self.partInfoSub = None
		if self.partInfoPub is not None:
			self.partInfoPub.stop()
			self.partInfoPub = None

	def start(self):
		if not self.shouldRun and self.domain.metaDataMcPort > 0:
			self.shouldRun = True
			thread.start_new_thread( self.run, () )

	def stop(self):
		if self.shouldRun:
			self.shouldRun = False

	def onParticipantInfoData(self,sub,message):
		addr,port = map(str,message.getSource())
		data=message.data
		tempStr = "(From " + addr + ":" + port
		tempStr +=") PartInfoData: Name: " + data.name
		tempStr +=",  languageImplementation: " + data.languageImplementation
#		print(tempStr)
