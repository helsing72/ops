import opsTypes
import ParticipantInfoData

class AbstractFactory(object):
	def __init__(self):
		super(AbstractFactory, self).__init__()
		pass
	def create(self,typesString):
		raise NotImplementedError


class BasicFactory(AbstractFactory):
	def __init__(self):
		super(BasicFactory, self).__init__()
		self.types = {}

	def addType(self,messageID,messageType):
		self.types[messageID] = messageType

	def create(self,typesString):
		#print "Factory:create",typesString
		for s in typesString.split(' '):
			for k, v in self.types.iteritems():
				if k == s:
					return v()
		return None

	def __str__(self):
		string = "Factory"
		for k, v in self.types.iteritems():
			string += "\n  " + k
		return string

class CompositFactory(AbstractFactory):
	def __init__(self):
		super(CompositFactory, self).__init__()
		self.factories = []
	def addFactory(self,factory):
		self.factories.append(factory)
	def create(self,typesString):
		for f in self.factories:
			obj = f.create(typesString)
			if obj is not None:
				return obj
		return None

class OpsDefaultFactory(CompositFactory):
	def __init__(self):
		super(OpsDefaultFactory, self).__init__()
		b = BasicFactory()
		b.addType('ops.protocol.OPSMessage',opsTypes.Message)
		b.addType('Topic',opsTypes.Topic)
		b.addType('Channel',opsTypes.Channel)
		b.addType('Transport',opsTypes.Transport)
		b.addType('Domain',opsTypes.Domain)
		b.addType("MulticastDomain",opsTypes.Domain)
		b.addType("DefaultOPSConfigImpl",opsTypes.DefaultOPSConfigImpl)
		b.addType("ops.ParticipantInfoData", ParticipantInfoData.ParticipantInfoData)
		self.addFactory(b)
