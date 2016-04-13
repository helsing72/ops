#Auto generated OPS-code. DO NOT MODIFY!
from  pizza import CapricosaData
from  pizza import PizzaData
from  pizza.special import Cheese
from  pizza.special import ExtraAllt
from  pizza.special import LHCData
from  pizza import VessuvioData


class PizzaProjectTypeFactory(BasicFactory):
	def __init__(self):
		super(PizzaProjectTypeFactory,self).__init__()
		self.addType("pizza.CapricosaData",CapricosaData)
		self.addType("pizza.PizzaData",PizzaData)
		self.addType("pizza.special.Cheese",Cheese)
		self.addType("pizza.special.ExtraAllt",ExtraAllt)
		self.addType("pizza.special.LHCData",LHCData)
		self.addType("pizza.VessuvioData",VessuvioData)
