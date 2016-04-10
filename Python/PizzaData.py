from ops.opsTypes import OPS_Object
from ops.Factory import BasicFactory


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

class VessuvioData(PizzaData):
	TypeName = "pizza.VessuvioData"
	def __init__(self):
		super(VessuvioData,self).__init__()
		self.appendType("pizza.VessuvioData")
		self.ham = ""
	def serialize(self,archiver):
		super(VessuvioData,self).serialize(archiver)
		self.ham = archiver.String("ham", self.ham)

class CapricosaData(VessuvioData):
	TypeName = "pizza.CapricosaData"
	def __init__(self):
		super(CapricosaData,self).__init__()
		self.appendType("pizza.CapricosaData")
		self.mushrooms = ""

	def serialize(self,archiver):
		super(CapricosaData,self).serialize(archiver)
		self.mushrooms = archiver.String("mushrooms", self.mushrooms)


class pizza_factory(BasicFactory):
	def __init__(self):
		super(pizza_factory,self).__init__()
		self.addType("pizza.PizzaData",PizzaData)
		self.addType("pizza.VessuvioData",VessuvioData)
		self.addType("pizza.CapricosaData",CapricosaData)