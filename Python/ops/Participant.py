from opsTypes import Config,Topic
from Factory import OpsDefaultFactory
import SendDataHandler
import ReceiveDataHandler
import ParticipantInfoData

from socket import gethostname
from os import getpid

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

		self.partInfoData = ParticipantInfoData.ParticipantInfoData()

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
		return SendDataHandler.getSendDataHandler(self,topic)
	def getReceiveDataHandler(self,topic):
		return ReceiveDataHandler.getReceiveDataHandler(self,topic)

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
