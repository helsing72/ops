import ops.opsTypes
import ops.ParticipantInfoData

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
		try:
			typs = self.types.iteritems()
		except AttributeError:
			typs = self.types.items()	# Python 2 and 3 difference
		for s in typesString.split(' '):
			for k, v in typs:
				if k == s:
					return v()
		return None

	def __str__(self):
		string = "Factory"
		try:
			typs = self.types.iteritems()
		except AttributeError:
			typs = self.types.items()	# Python 2 and 3 difference
		for k, v in typs:
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
		b.addType('ops.protocol.OPSMessage',ops.opsTypes.Message)
		b.addType('Topic',ops.opsTypes.Topic)
		b.addType('Channel',ops.opsTypes.Channel)
		b.addType('Transport',ops.opsTypes.Transport)
		b.addType('Domain',ops.opsTypes.Domain)
		b.addType("MulticastDomain",ops.opsTypes.Domain)
		b.addType("DefaultOPSConfigImpl",ops.opsTypes.DefaultOPSConfigImpl)
		b.addType("ops.ParticipantInfoData", ops.ParticipantInfoData.ParticipantInfoData)
		self.addFactory(b)
