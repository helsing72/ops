import time
import re
import sys
import os
import platform


sys.path.append("/home/pane/slask/jali-ops/ops-development/Python")
from ops import Participant,Publisher,Subscriber,Print_Archiver

curDir = os.getcwd()
sys.path.append(curDir+"/../Pizzas")

import pizza
import pizza_special
import PizzaProjectTypeFactory
import InitData

msleep = lambda x: time.sleep(x/1000.0)


class CHelper(object):
	def __init__(self):
		super(CHelper,self).__init__()
		self.callback = None
		self.pub = None
		self.sub = None
		self.expectedPubId = -1
		self.data = None

	def CreatePublisher(self,part,topicName):
		if self.pub is not None:
			print "Publisher already exist for topic " + self.pub.getTopic().getName()
		else:
			topic = part.createTopic(topicName)
			self.pub = Publisher.Publisher(topic)
			self.pub.name = "PythonTest " + platform.system() + ("(%s)" % os.getpid())

	def DeletePublisher(self,doLog = True):
		del self.pub

	def StartPublisher(self):
		self.pub.start()

	def StopPublisher(self):
		self.pub.stop()

	def Write(self,pizzaSize):
		if pizzaSize == "normal":
			self.data = InitData.initExtraAlltNormal()
			print "sending normal"
		if pizzaSize == "large":
			self.data = InitData.initExtraAlltLarge()
			print "sending large"
		
		self.pub.write(self.data)

class ItemInfo(object):
	def __init__(self,dom,top,typ):
		self.Domain = dom
		self.TopicName = top
		self.TypeName = typ
		self.helper = None
		self.part = None
		self.selected = False
	def __str__(self):
		return ("P" if self.helper.HasPublisher() else " ") + ("S" if self.helper.HasPublisher() else " ") + ("*" if self.selected else " ") + self.Domain + "::" + self.TopicName

info = ItemInfo("PizzaDomain", "ExtraAlltTopic", "pizza.special.ExtraAllt")

participant = Participant.Participant.getInstance("PizzaDomain", "PizzaDomain", "../ops_config.xml")
if participant == None:
	print "Failed to create Participant. Missing ops_config.xml ??"
	sys.exit(-1)
participant.addTypeSupport(PizzaProjectTypeFactory.PizzaProjectTypeFactory())

						
info.helper = CHelper()
info.helper.data = pizza_special.ExtraAllt()
info.part = participant

info.helper.CreatePublisher(info.part, info.TopicName)

info.helper.StartPublisher()

info.helper.Write("normal")
msleep(1000)


info.helper.Write("large")
msleep(1000)

for index in range(1,11):
	info.helper.Write("normal")
	msleep(10)

msleep(1000)
for index in range(1,11):
	info.helper.Write("large")
	msleep(100)
