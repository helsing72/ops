from ops.Archiver import Archiver_Out

class Print_Archiver_Out(Archiver_Out):

	def __init__(self):
		super(Print_Archiver_Out, self).__init__()
		self.currentTabDepth=0;

	def tab(self):
		return "   " * self.currentTabDepth
	def printObject(self, name, obj):
		print("________________Begin Object___________________")
		self.Ops(name, obj)
		print("_________________End Object____________________")

	def Bool(self,name,value):
		print(self.tab() + name + " = " + str(value))
		return value
	def Int8(self,name,value):
		print(self.tab() + name + " = " + str(value))
		return value
	def Int16(self,name,value):
		print(self.tab() + name + " = " + str(value))
		return value
	def Int32(self,name,value):
		print(self.tab() + name + " = " + str(value))
		return value
	def Int64(self,name,value):
		print(self.tab() + name + " = " + str(value))
		return value
	def Float32(self,name,value):
		print(self.tab() + name + " = " + str(value))
		return value
	def Float64(self,name,value):
		print(self.tab() + name + " = " + str(value))
		return value
	def String(self,name,value):
		print(self.tab() + name + " = " + value)
		return value
	def Ops(self,name,value,prototype=None):
		if value is not None:
			print(self.tab() + name + " type = " + value.typesString)
			self.currentTabDepth+=1
			value.serialize(self)
			self.currentTabDepth-=1
		return value

	def BoolVector(self,name,value):
		if len(value) > 0:
			print(self.tab() + name + '(size = ' + str(len(value)) + ') = [ ' + str(value[0]) + ' ... ' + str(value[-1]) + ' ]')
	def Int8Vector(self,name,value):
		if len(value) > 0:
			print(self.tab() + name + '(size = ' + str(len(value)) + ') = [ ' + str(value[0]) + ' ... ' + str(value[-1]) + ' ]')
	def Int16Vector(self,name,value):
		if len(value) > 0:
			print(self.tab() + name + '(size = ' + str(len(value)) + ') = [ ' + str(value[0]) + ' ... ' + str(value[-1]) + ' ]')
	def Int32Vector(self,name,value):
		if len(value) > 0:
			print(self.tab() + name + '(size = ' + str(len(value)) + ') = [ ' + str(value[0]) + ' ... ' + str(value[-1]) + ' ]')
	def Int64Vector(self,name,value):
		if len(value) > 0:
			print(self.tab() + name + '(size = ' + str(len(value)) + ') = [ ' + str(value[0]) + ' ... ' + str(value[-1]) + ' ]')
	def Float32Vector(self,name,value):
		if len(value) > 0:
			print(self.tab() + name + '(size = ' + str(len(value)) + ') = [ ' + str(value[0]) + ' ... ' + str(value[-1]) + ' ]')
	def Float64Vector(self,name,value):
		if len(value) > 0:
			print(self.tab() + name + '(size = ' + str(len(value)) + ') = [ ' + str(value[0]) + ' ... ' + str(value[-1]) + ' ]')
	def StringVector(self,name,value):
		if len(value) > 0:
			print(self.tab() + name + '(size = ' + str(len(value)) + ') = [ "' + value[0] + '" ... "' + value[-1] + '" ]')
	def OpsVector(self,name,value,prototype=None):
		print(self.tab() + name + " = ")
		self.currentTabDepth+=1
		for v in value:
			self.Ops("element", v,prototype)
		self.currentTabDepth-=1
	def Spare(self,data):
		raise NotImplementedError