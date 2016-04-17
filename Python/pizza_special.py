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

	def validate(self):
		super(Cheese,self).validate()
		print "Checking self.name for string"
		if not isinstance(self.name,str):
			raise ValueError()
		print "Checking self.age for double"
		if not isinstance(self.age,float):
			raise ValueError()

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
		archiver.OpsVector("p", self.p, PizzaData)

	def validate(self):
		super(LHCData,self).validate()
		print "Checking self.bearnaise for string"
		if not isinstance(self.bearnaise,str):
			raise ValueError()
		print "Checking self.beef for string"
		if not isinstance(self.beef,str):
			raise ValueError()
		print "Checking self.p for pizza.PizzaData[]"
		for x in self.p:
			if not isinstance(x,PizzaData) or not x.validate():
				raise ValueError()

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
		archiver.BoolVector("bools", self.bools)
		archiver.Int8Vector("bytes", self.bytes)
		archiver.Int32Vector("ints", self.ints)
		archiver.Int64Vector("longs", self.longs)
		archiver.Float32Vector("floats", self.floats)
		archiver.Float64Vector("doubles", self.doubles)
		archiver.StringVector("strings", self.strings)
		archiver.OpsVector("cheeses", self.cheeses, Cheese)

	def validate(self):
		super(ExtraAllt,self).validate()
		print "Checking self.extraCheese for boolean"
		if not isinstance(self.extraCheese,bool):
			raise ValueError()
		print "Checking self.nrOfMushRooms for byte"
		if not isinstance(self.nrOfMushRooms,int):
			raise ValueError()
		print "Checking self.meetQuality for int"
		if not isinstance(self.meetQuality,int):
			raise ValueError()
		print "Checking self.timestamp for long"
		if not isinstance(self.timestamp,(int,long)):
			raise ValueError()
		print "Checking self.timeBakedHours for float"
		if not isinstance(self.timeBakedHours,float):
			raise ValueError()
		print "Checking self.timeBakedSeconds for double"
		if not isinstance(self.timeBakedSeconds,float):
			raise ValueError()
		print "Checking self.description for string"
		if not isinstance(self.description,str):
			raise ValueError()
		print "Checking self.cheese_ for pizza.special.Cheese"
		if not isinstance(self.cheese_,Cheese) or not self.cheese_.validate():
			raise ValueError()
		print "Checking self.bools for boolean[]"
		for x in self.bools:
			if not isinstance(x,bool):
				raise ValueError()
		print "Checking self.bytes for byte[]"
		for x in self.bytes:
			if not isinstance(x,int):
				raise ValueError()
		print "Checking self.ints for int[]"
		for x in self.ints:
			if not isinstance(x,int):
				raise ValueError()
		print "Checking self.longs for long[]"
		for x in self.longs:
			if not isinstance(x,(int,long)):
				raise ValueError()
		print "Checking self.floats for float[]"
		for x in self.floats:
			if not isinstance(x,float):
				raise ValueError()
		print "Checking self.doubles for double[]"
		for x in self.doubles:
			if not isinstance(x,float):
				raise ValueError()
		print "Checking self.strings for string[]"
		for x in self.strings:
			if not isinstance(x,str):
				raise ValueError()
		print "Checking self.cheeses for pizza.special.Cheese[]"
		for x in self.cheeses:
			if not isinstance(x,Cheese) or not x.validate():
				raise ValueError()

