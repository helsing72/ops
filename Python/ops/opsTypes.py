#import Factory
#from XML_Archiver import XML_Archiver_In
from Constants import *
import copy

class OPS_Object(object):
	"""docstring for OPS_Object"""
	def __init__(self):
		super(OPS_Object, self).__init__()
		self.key = "k"
		self.typesString = ""
		self.spareBytes = None

	def __str__(self):
		return "OPS_Object:\n  key=%s\n  typesString=%s" % (self.key,self.typesString)

	def serialize(self,dataBuffer):
		self.key = dataBuffer.String("key",self.key)

	def appendType(self,str):
		self.typesString = str + " " + self.typesString

class Message(OPS_Object):
	"""docstring for Message"""
	def __init__(self,data=None):
		super(Message,self).__init__()
		self.appendType("ops.protocol.OPSMessage")

		self.messageType=0
		self.publisherPriority=0
		self.publicationID=0
		self.publisherName=""
		self.topicName=""
		self.topLevelKey=""
		self.address=""
		self.data=data
		self.sourceAddr=("",0)

	def __str__(self):
		temp = ""
		if self.printable:
			temp = super(OPS_Message,self).__str__() + "\nOPS_Message:\n  messageType=%s\n  publisherPriority=%s\n  publicationID=%s\n  publisherName=%s\n  topicName=%s\n  topLevelKey=%s\n  address=%s\n" % (self.messageType,self.publisherPriority,self.publicationID,self.publisherName,self.topicName,self.topLevelKey,self.address)

		if self.data is not None:
			return temp + self.data.__str__()
		else:
			return temp

	def serialize(self,dataBuffer):
		super(Message,self).serialize(dataBuffer)
		self.messageType = dataBuffer.Int8("messageType",self.messageType)
		self.publisherPriority = dataBuffer.Int8("publisherPriority",self.publisherPriority)
		self.publicationID = dataBuffer.Int64("publicationID",self.publicationID)
		self.publisherName = dataBuffer.String("publisherName",self.publisherName)
		self.topicName = dataBuffer.String("topicName",self.topicName)
		self.topLevelKey = dataBuffer.String("topLevelKey",self.topLevelKey)
		self.address = dataBuffer.String("address",self.address)
		self.data = dataBuffer.Ops("data",self.data)

	def setSource(self,addr):
		self.sourceAddr = addr

	def getSource(self):
		return self.sourceAddr


class Topic(OPS_Object):
	"""docstring for Topic"""
	def __init__(self):
		super(Topic,self).__init__()
		self.appendType("Topic");
		self.name=""
		self.typeID=""
		self.port=0
		self.domainAddress=""
		self.outSocketBufferSize=-1
		self.inSocketBufferSize=-1
		self.sampleMaxSize=PACKET_MAX_SIZE
		self.transport=TRANSPORT_MC
		self.participant=None

	def __str__(self):
		temp ="\nTopic:"
		temp += "\n  name: %s" % self.name
		temp += "\n  typeID: %s" % self.typeID
		temp += "\n  port: %s" % self.port
		temp += "\n  domainAddress: %s" % self.domainAddress
		temp += "\n  outSocketBufferSize: %s" % self.outSocketBufferSize
		temp += "\n  inSocketBufferSize: %s" % self.inSocketBufferSize
		temp += "\n  sampleMaxSize: %s" % self.sampleMaxSize
		temp += "\n  transport: %s" % self.transport
		return  super(Topic,self).__str__() + temp

	def setSampleMaxSize(self,size):
		self.sampleMaxSize =  max(PACKET_MAX_SIZE,size)

	def serialize(self,archiver):
		super(Topic,self).serialize(archiver)
		self.name = archiver.String("name",self.name)
		self.typeID = archiver.String("dataType",self.typeID)
		self.port = archiver.Int32("port",self.port)
		self.domainAddress = archiver.String("address",self.domainAddress)
		self.outSocketBufferSize = archiver.Int64("outSocketBufferSize",self.outSocketBufferSize)
		self.inSocketBufferSize = archiver.Int64("inSocketBufferSize",self.inSocketBufferSize)

		temp = self.sampleMaxSize
		temp = archiver.Int32("sampleMaxSize",temp)
		self.setSampleMaxSize(temp)

		self.transport = archiver.String("transport",self.transport)
		if self.transport=="":
			self.transport = TRANSPORT_MC
	def getName(self):
		return self.name

class Domain(OPS_Object):
	"""docstring for Topic"""	
	def __init__(self):
		super(Domain,self).__init__()
		self.appendType("Domain")
		self.domainAddress=""
		self.timeToLive = 1
		self.localInterface = "0.0.0.0"
		self.inSocketBufferSize = 16000000
		self.outSocketBufferSize = 16000000
		self.metaDataMcPort = 9494
		self.topics=[]
		self.domainID=""

	def __str__(self):
		temp ="\nDomain:"
		temp += "\n  domainAddress: %s" % self.domainAddress
		temp += "\n  timeToLive: %s" % self.timeToLive
		temp += "\n  localInterface: %s" % self.localInterface
		temp += "\n  inSocketBufferSize: %s" % self.inSocketBufferSize
		temp += "\n  outSocketBufferSize: %s" % self.outSocketBufferSize
		temp += "\n  metaDataMcPort: %s" % self.metaDataMcPort
		temp += "\n  domainID: %s" % self.domainID

		for t in self.topics:
		 	temp+="\n" + t.__str__()
		#temp += "\n  topics: %s" % self.topics
		return super(Domain,self).__str__() + temp



	def serialize(self,archiver):
		super(Domain,self).serialize(archiver)
		self.domainID = archiver.String("domainID",self.domainID)
		archiver.OpsVector("topics",self.topics,Topic)
		self.domainAddress = archiver.String("domainAddress",self.domainAddress)
		self.localInterface = archiver.String("localInterface",self.localInterface)
		self.timeToLive = archiver.Int32("timeToLive",self.timeToLive)
		self.inSocketBufferSize = archiver.Int32("inSocketBufferSize",self.inSocketBufferSize)
		self.outSocketBufferSize = archiver.Int32("outSocketBufferSize",self.outSocketBufferSize)
		self.metaDataMcPort = archiver.Int32("metaDataMcPort",self.metaDataMcPort)

	def getTopic(self,name):
		for t in self.topics:
			if t.domainAddress == "":
				t.domainAddress = self.domainAddress
			if t.name == name:
				return copy.deepcopy(t)
		return None


import Factory
from XML_Archiver import XML_Archiver_In


class Config(OPS_Object):
	def __init__(self):
		super(Config, self).__init__()
		self.domains=[]

	def __str__(self):
		temp = "Config:"
		for d in self.domains:
			temp += "\n" +  d.__str__()
		return super(Config,self).__str__() + "\n" + temp


	@staticmethod
	def getConfig(filename="ops_config.xml"):
		xmlArchiver = XML_Archiver_In(Factory.OpsDefaultFactory(),filename,"root")
		config = xmlArchiver.Ops("ops_config",None)
		return config


	def getDomain(self,domainID):
		for d in self.domains:
			if d.domainID == domainID:
				return d
		return None


	def serialize(self,dataBuffer):
		super(Config,self).serialize(dataBuffer)
		dataBuffer.OpsVector("domains",self.domains,Domain)


class DefaultOPSConfigImpl(Config):
	def __init__(self):
		super(DefaultOPSConfigImpl, self).__init__()
		self.appendType("DefaultOPSConfigImpl")