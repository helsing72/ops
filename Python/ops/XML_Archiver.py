from Archiver import Archiver_In,Archiver_Out
import xml.etree.ElementTree as ET
#import ops

class XML_Archiver_In(Archiver_In):
	def __init__(self,factory,filename,rootName):
		super(XML_Archiver_In, self).__init__(factory)
		tree = ET.parse(filename)
		self.stack = [tree.getroot()]

	def Int8(self,name,value):
		newNode = self.stack[-1].find(name)
		if newNode is not None:
			value = int(newNode.text)
		return value

	def Int16(self,name,value):
		newNode = self.stack[-1].find(name)
		if newNode is not None:
			value = int(newNode.text)
		return value

	def Int32(self,name,value):
		newNode = self.stack[-1].find(name)
		if newNode is not None:
			value = int(newNode.text)
		return value

	def Int64(self,name,value):
		newNode = self.stack[-1].find(name)
		if newNode is not None:
			value = int(newNode.text)
		return value

	def Float32(self,name,value):
		newNode = self.stack[-1].find(name)
		if newNode is not None:
			value = float(newNode.text)
		return value

	def Float64(self,name,value):
		newNode = self.stack[-1].find(name)
		if newNode is not None:
			value = float(newNode.text)
		return value

	def String(self,name,value):
		newNode = self.stack[-1].find(name)
		if newNode is not None:
			value = newNode.text
		return value

	def __Ops(self,value,prototype=None):
		typename = self.stack[-1].get("type")
		if prototype is not None:
			value = prototype()
		else:
	 		value = self.factory.create(typename)
	 	
	 	if value is not None:
	 		value.serialize(self)
	 	return value


	def Ops(self,name,value,prototype=None):
		newNode = self.stack[-1].find(name);
		if newNode is not None:
			self.stack.append(newNode)
			value = self.__Ops(value,prototype)
		 	self.stack.pop()
		return value

	def OpsVector(self,name,values,prototype=None):
		del values[:]
		newNode = self.stack[-1].find(name);
		if newNode is not None:
			self.stack.append(newNode)
			for node in self.stack[-1].findall('element'):
				self.stack.append(node)		 		
		 		data = self.__Ops(None,prototype)
		 		values.append(data)		 		
		 		self.stack.pop()
		 	self.stack.pop()

class XML_Archiver_Out(Archiver_Out):
	def __init__(self,filename,rootName):
		super(XML_Archiver_Out, self).__init__()
		self.file = open(filename,"w")
		self.depth = 0
		self.rootName=rootName
		self.beginTag(rootName)

	def __del__(self):
		self.closeTag(self.rootName)
		self.file.close()

	def getIndent(self):
		return "\t" * self.depth

	def beginTag(self,name,attr = {}):
		text = self.getIndent() + ("<%s"  % name)
		for k in attr:
			text += (' %s = "%s"') % (k,attr[k])
		text += ">\n"
		self.file.write(text)
		self.depth += 1
	
	def closeTag(self,name="root"):
		self.depth -= 1
		text = self.getIndent() + ("</%s>\n"  % name)
		self.file.write(text)

 	def Bool(self,name,value):
 		self.String(name,str(value))
 		return value
 	def Int8(self,name,value):
 		self.String(name,str(value))
 		return value
 	def Int16(self,name,value):
 		self.String(name,str(value))
 		return value
 	def Int32(self,name,value):
 		self.String(name,str(value))
 		return value
 	def Int64(self,name,value):
 		self.String(name,str(value))
 		return value
 	def Float32(self,name,value):
 		self.String(name,str(value))
 		return value
 	def Float64(self,name,value):
 		self.String(name,str(value))
 		return value
 	def String(self,name,value):
 		text = self.getIndent() + "<%s>%s</%s>\n" % (name,value,name)
 		self.file.write(text)
 		return value
 	def Ops(self,name,value,prototype=None):
 		self.beginTag(name,{'type' : value.typesString})
 		value.serialize(self)
 		self.closeTag(name)
 		return value

 	def BoolVector(self,name,value):
 		self.beginTag(name)
 		for v in value:
 			self.String("element",str(v))
 		self.closeTag(name)
 	def Int8Vector(self,name,value):
 		self.beginTag(name)
 		for v in value:
 			self.String("element",str(v))
 		self.closeTag(name)
 	def Int16Vector(self,name,value):
 		self.beginTag(name)
 		for v in value:
 			self.String("element",str(v))
 		self.closeTag(name)
 	def Int32Vector(self,name,value):
 		self.beginTag(name)
 		for v in value:
 			self.String("element",str(v))
 		self.closeTag(name)
 	def Int64Vector(self,name,value):
 		self.beginTag(name)
 		for v in value:
 			self.String("element",str(v))
 		self.closeTag(name)
 	def Float32Vector(self,name,value):
 		self.beginTag(name)
 		for v in value:
 			self.String("element",str(v))
 		self.closeTag(name)
 	def Float64Vector(self,name,value):
 		self.beginTag(name)
 		for v in value:
 			self.String("element",str(v))
 		self.closeTag(name)
 	def StringVector(self,name,value):
 		self.beginTag(name)
 		for v in value:
 			self.String("element",v)
 		self.closeTag(name)
 	def OpsVector(self,name,value,prototype=None):
 		self.beginTag(name)
 		for v in value:
 			self.Ops("element",v)
 		self.closeTag(name)