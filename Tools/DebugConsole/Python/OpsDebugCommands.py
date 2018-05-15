import time
import re
import sys
import os
import platform

import opsidls
import opsidlsTypeFactory
from ops import Participant,Publisher,Subscriber,Print_Archiver

beQuite = False
showCommands = False

def ShowCommands(sub,mess):
	if showCommands:
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

def ShowResponse(sub,mess):
	addr,port = map(str,mess.getSource())
	dumpInfo = True
	data=mess.data
	if data.Entity == 0:	# Debug
		if data.Result1 == 2:
			print "  Publishers:"
			dumpInfo = False
		if data.Result1 == 3:
			print "  Subscribers:"
			dumpInfo = False
		if not dumpInfo:
			for name in data.Param3:
				print "    " + name

	if data.Entity == 2:	# Publisher
		dumpInfo = False
		tempStr = " Key: " + data.key
		tempStr +=" Publisher Name: " + data.Name
		tempStr +=" Enabled: " + str(data.Enabled)
		tempStr +=" PubId: " + str(data.Result1)
		print tempStr

	if data.Entity == 3:	# Subscriber
		dumpInfo = False
		tempStr = " Key: " + data.key
		tempStr +=" Subscriber Name: " + data.Name
		tempStr +=" Enabled: " + str(data.Enabled)
		tempStr +=" #Mess: " + str(data.Result1)
		print tempStr

	if dumpInfo:
		tempStr = "[Topic: " + sub.getTopic().getName()
		tempStr +="] (From " + addr + ":" + port
		tempStr +=")"
		tempStr +=" Entity: " + str(data.Entity)
		tempStr +=" Name: " + data.Name
		tempStr +=" Command: " + str(data.Command)
		tempStr +=" Result1: " + str(data.Result1)
		print tempStr

def onDebugRequestResponseData(sub,mess):
	if not beQuite:
		data=mess.data
		if data.Command == 0:
			ShowResponse(sub,mess)
		else:
			ShowCommands(sub,mess)

class OpsDebugCommands():
	def __init__(self,part):
		self.pub = None
		self.sub = None
		self.request = opsidls.DebugRequestResponseData()
		part.addTypeSupport(opsidlsTypeFactory.opsidlsTypeFactory())
		##
		topic = part.createDebugTopic()
		print "Created topic " + topic.name + " [" + topic.transport + "." + topic.domainAddress + "." + str(topic.port) + " (IF:" + topic.localInterface + ")]"
		if topic.port == 0:
			print ""
			print "!!!! DebugTopic NOT enabled in given Domain !!!!"

		self.pub = Publisher.Publisher(topic)
		self.pub.name = "PyDbgConsole"
		self.pub.start();
		##
		self.sub = Subscriber.Subscriber(topic)
		self.sub.addCallback(onDebugRequestResponseData)
		self.sub.start()

	def Write(self,data):
		self.pub.write(data);

	############################
	def Command(self,key,ent,cmd,param):
		self.request.key = key
		self.request.Entity = ent
		self.request.Command = cmd
		self.request.Param1 = param
		self.Write(self.request)

	############################
	def DebugCommand(self,key,cmd,param):
		self.Command(key,0,cmd,param)

	def ListPublishers(self,key):
		self.DebugCommand(key,2,2)

	def ListSubscribers(self,key):
		self.DebugCommand(key,2,3)

	############################
	def PublisherCommand(self,key,top,cmd,param):
		self.request.Name = top
		self.Command(key,2,cmd,param)

	def PublisherStatus(self,key,topic):
		self.PublisherCommand(key,topic,1,0)

	def DisablePublisher(self,key,topic):
		self.PublisherCommand(key,topic,2,0)

	def EnablePublisher(self,key,topic):
		self.PublisherCommand(key,topic,2,1)

	def IncrementPubId(self,key,topic,amount):
		self.PublisherCommand(key,topic,3,amount)

	############################
	def SubscriberCommand(self,key,top,cmd,param):
		self.request.Name = top
		self.Command(key,3,cmd,param)

	def SubscriberStatus(self,key,topic):
		self.SubscriberCommand(key,topic,1,0)

	def DisableSubscriber(self,key,topic):
		self.SubscriberCommand(key,topic,2,0)

	def EnableSubscriber(self,key,topic):
		self.SubscriberCommand(key,topic,2,1)
