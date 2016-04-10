from ops.opsTypes import OPS_Object

class XdrDebug(OPS_Object):
	def __init__(self):
		super(XdrDebug,self).__init__()
		self.appendType("PY.XdrDebug")
		self.message = ""
	
	def __str__(self):
		return "XdrDebug:\n  message=" + self.message
	
	def serialize(self,archiver):
		super(XdrDebug,self).serialize(archiver)
		self.message = archiver.String("message",self.message)

class XdrSignedDebug(OPS_Object):
	def __init__(self):
		super(XdrSignedDebug,self).__init__()
		self.appendType("PY.XdrSignedDebug")
		self.signitures = []
		self.values = []
		self.messages = []

	def serialize(self,archiver):
		super(XdrSignedDebug,self).serialize(archiver)
		archiver.StringVector("signitures",self.signitures)
		archiver.Int8Vector("values",self.values)
		archiver.OpsVector("messages",self.messages,XdrDebug)