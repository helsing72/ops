class __className(__baseClassName):
	TypeName = "__packageName.__className"
__declarations
	def __init__(self):
		super(__className,self).__init__()
		self.appendType("__packageName.__className")
		self.value = 0
	def serialize(self,archiver):
		super(__className,self).serialize(archiver)
		self.value = archiver.Int32("value",self.value)
	def validate(self):
		super(__className,self).validate()
		if not isinstance(self.value,int):
			raise ValueError()
		if self.value<0 or self.value>=__validation:
			raise ValueError()

