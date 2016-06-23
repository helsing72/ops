#Auto generated OPS-code. DO NOT MODIFY!
from ops.opsTypes import OPS_Object

class PizzaData(OPS_Object):
	TypeName = "pizza.PizzaData"
	def __init__(self):
		super(PizzaData,self).__init__()
		self.appendType("pizza.PizzaData")
		self.cheese = ""
		self.tomatoSauce = ""

	def serialize(self,archiver):
		super(PizzaData,self).serialize(archiver)
		self.cheese = archiver.String("cheese", self.cheese)
		self.tomatoSauce = archiver.String("tomatoSauce", self.tomatoSauce)

	def validate(self):
		super(PizzaData,self).validate()
		if not isinstance(self.cheese,str):
			raise ValueError()
		if not isinstance(self.tomatoSauce,str):
			raise ValueError()

class VessuvioData(PizzaData):
	TypeName = "pizza.VessuvioData"
	def __init__(self):
		super(VessuvioData,self).__init__()
		self.appendType("pizza.VessuvioData")
		self.ham = ""

	def serialize(self,archiver):
		super(VessuvioData,self).serialize(archiver)
		self.ham = archiver.String("ham", self.ham)

	def validate(self):
		super(VessuvioData,self).validate()
		if not isinstance(self.ham,str):
			raise ValueError()

class CapricosaData(VessuvioData):
	TypeName = "pizza.CapricosaData"
	def __init__(self):
		super(CapricosaData,self).__init__()
		self.appendType("pizza.CapricosaData")
		self.mushrooms = ""

	def serialize(self,archiver):
		super(CapricosaData,self).serialize(archiver)
		self.mushrooms = archiver.String("mushrooms", self.mushrooms)

	def validate(self):
		super(CapricosaData,self).validate()
		if not isinstance(self.mushrooms,str):
			raise ValueError()

