#Auto generated OPS-code. DO NOT MODIFY!
from ops.opsTypes import OPS_Object
from pizza import CapricosaData
from pizza import PizzaData

class Cheese(OPS_Object):
	TypeName = "pizza.special.Cheese"
	def __init__(self):
		super(Cheese,self).__init__()
		self.appendType("pizza.special.Cheese")
		self.name = ""
		self.age = 0.0

	def serialize(self,archiver):
		super(Cheese,self).serialize(archiver)
		self.name = archiver.String("name", self.name)
		self.age = archiver.Float64("age", self.age)


class LHCData(CapricosaData):
	TypeName = "pizza.special.LHCData"
	def __init__(self):
		super(LHCData,self).__init__()
		self.appendType("pizza.special.LHCData")
		self.bearnaise = ""
		self.beef = ""
		self.p = []

	def serialize(self,archiver):
		super(LHCData,self).serialize(archiver)
		self.bearnaise = archiver.String("bearnaise", self.bearnaise)
		self.beef = archiver.String("beef", self.beef)
		self.p = archiver.OpsVector("p", self.p, PizzaData)


class ExtraAllt(LHCData):
	TypeName = "pizza.special.ExtraAllt"
	def __init__(self):
		super(ExtraAllt,self).__init__()
		self.appendType("pizza.special.ExtraAllt")
		#Does the order include extra cheese???
		self.extraCheese = False
		#@limits(0,INFINITY)
		self.nrOfMushRooms = 0
		self.meetQuality = 0
		self.timestamp = 0
		self.timeBakedHours = 0.0
		self.timeBakedSeconds = 0.0
		self.description = ""
		self.cheese_ = Cheese()
		self.bools = []
		self.bytes = []
		self.ints = []
		self.longs = []
		self.floats = []
		self.doubles = []
		self.strings = []
		self.cheeses = []

	def serialize(self,archiver):
		super(ExtraAllt,self).serialize(archiver)
		self.extraCheese = archiver.Bool("extraCheese", self.extraCheese)
		self.nrOfMushRooms = archiver.Int8("nrOfMushRooms", self.nrOfMushRooms)
		self.meetQuality = archiver.Int32("meetQuality", self.meetQuality)
		self.timestamp = archiver.Int64("timestamp", self.timestamp)
		self.timeBakedHours = archiver.Float32("timeBakedHours", self.timeBakedHours)
		self.timeBakedSeconds = archiver.Float64("timeBakedSeconds", self.timeBakedSeconds)
		self.description = archiver.String("description", self.description)
		self.cheese_ = archiver.Ops("cheese_", self.cheese_)
		self.bools = archiver.BoolVector("bools", self.bools)
		self.bytes = archiver.Int8Vector("bytes", self.bytes)
		self.ints = archiver.Int32Vector("ints", self.ints)
		self.longs = archiver.Int64Vector("longs", self.longs)
		self.floats = archiver.Float32Vector("floats", self.floats)
		self.doubles = archiver.Float64Vector("doubles", self.doubles)
		self.strings = archiver.StringVector("strings", self.strings)
		self.cheeses = archiver.OpsVector("cheeses", self.cheeses, Cheese)


