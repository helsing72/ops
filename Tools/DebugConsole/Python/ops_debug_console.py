import time
import re
import sys
import os
import platform

import opsidls
import opsidlsTypeFactory
from ops import Participant,Publisher,Subscriber,Print_Archiver

beQuite = False

def onDebugRequestResponseData(sub,mess):
	if not beQuite:
		addr,port = map(str,mess.getSource())
		data=mess.data
		tempStr = "[Topic: " + sub.getTopic().getName()
		tempStr +="] (From " + addr + ":" + port
		tempStr +=")"
		tempStr +=" Entity: " + str(data.Entity)
		tempStr +=" Name: " + data.Name
		tempStr +=" Command: " + str(data.Command)
		tempStr +=" Result1: " + str(data.Result1)
		print tempStr
		if data.Entity == 0:
			if data.Result1 == 2:
				print "  Publishers:"
			if data.Result1 == 3:
				print "  Subscribers:"
			for name in data.Param3:
				print "    " + name


class CDebugHandler():
	def __init__(self,part):
		self.pub = None
		self.sub = None
		self.request = opsidls.DebugRequestResponseData()
		self.key = ""
		##
		topic = part.createTopic("ops.DebugTopic");
		print "Created topic " + topic.name + " [" + topic.transport + "." + topic.domainAddress +"." + str(topic.port) + "]"
		self.pub = Publisher.Publisher(topic)
		self.pub.name = "PyDbgConsole"
		self.pub.start();
		##
		self.sub = Subscriber.Subscriber(topic)
		self.sub.addCallback(onDebugRequestResponseData)
		self.sub.start()

	def Write(self,data):
		self.pub.write(data);

	def SetKey(self,key):
		self.key = key

	############################
	def Command(self,ent,cmd,param,key=""):
		if key == "":
			self.request.key = self.key
		else:
			self.request.key = key
		self.request.Entity = ent
		self.request.Command = cmd
		self.request.Param1 = param
		self.Write(self.request)

	############################
	def DebugCommand(self,cmd,param,key=""):
		self.Command(0,cmd,param,key)

	def ListPublishers(self,key=""):
		self.DebugCommand(2,2,key)

	def ListSubscribers(self,key=""):
		self.DebugCommand(2,3,key)

	############################
	def PublisherCommand(self,top,cmd,param,key=""):
		self.request.Name = top
		self.Command(2,cmd,param,key)

	def PublisherStatus(self,topic,key=""):
		self.PublisherCommand(topic,1,0,key)

	def DisablePublisher(self,topic,key=""):
		self.PublisherCommand(topic,2,0,key)

	def EnablePublisher(self,topic,key=""):
		self.PublisherCommand(topic,2,1,key)

	def IncrementPubId(self,topic,amount,key=""):
		self.PublisherCommand(topic,3,amount,key)

	############################
	def SubscriberCommand(self,top,cmd,param,key=""):
		self.request.Name = top
		self.Command(3,cmd,param,key)

	def SubscriberStatus(self,topic,key=""):
		self.SubscriberCommand(topic,1,0,key)

	def DisableSubscriber(self,topic,key=""):
		self.PublisherCommand(topic,2,0,key)

	def EnableSubscriber(self,topic,key=""):
		self.PublisherCommand(topic,2,1,key)



###############################################

participant = Participant.Participant.getInstance("PizzaDomain", "PizzaDomain")
if participant == None:
	print "Failed to create Participant. Missing ops_config.xml ??"
	sys.exit(-1)
participant.addTypeSupport(opsidlsTypeFactory.opsidlsTypeFactory())

dbgHandler = CDebugHandler(participant)
dbgHandler.SetKey("Pizza2")

commands = re.split(" |\t", raw_input(" List > ").upper())
dbgHandler.ListPublishers()
dbgHandler.ListSubscribers(key = "Pizza")

commands = re.split(" |\t", raw_input(" Disable publisher > ").upper())
dbgHandler.DisablePublisher(key = "Pizza", topic = "PizzaTopic")

commands = re.split(" |\t", raw_input(" Enable publisher > ").upper())
dbgHandler.EnablePublisher(key = "Pizza", topic = "PizzaTopic")

commands = re.split(" |\t", raw_input(" Inc PubID > ").upper())
dbgHandler.IncrementPubId(key = "Pizza", topic = "PizzaTopic", amount = 5)

commands = re.split(" |\t", raw_input(" Disable subscriber > ").upper())
dbgHandler.DisableSubscriber("PizzaTopic")

commands = re.split(" |\t", raw_input(" Enable subscriber > ").upper())
dbgHandler.EnableSubscriber("PizzaTopic")

commands = re.split(" |\t", raw_input(" (? = menu) > ").upper())
