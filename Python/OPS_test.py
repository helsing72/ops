import time
import re
import sys
import os
import platform

import PizzaData
from ops import Participant,Publisher,Subscriber,Print_Archiver

class Listener(object):
	def __init__(self):
		super(Listener,self).__init__()

	def onNewData(self,obj):
		pa = Print_Archiver.Print_Archiver_Out()
		pa.printObject("Test",obj.data)



class IHelper(object):
	def __init__(self):
		super(IHelper,self).__init__()
		pass
	def HasPublisher(self):
		raise NotImplementedError
	def HasSubscriber(self):
		raise NotImplementedError
	def CreatePublisher(self,part,topicName):
		raise NotImplementedError
	def DeletePublisher(self,doLog = True):
		raise NotImplementedError
	def StartPublisher(self):
		raise NotImplementedError
	def StopPublisher(self):
		raise NotImplementedError
	def Write(self):
		raise NotImplementedError
	def CreateSubscriber(self,part,topicName):
		raise NotImplementedError
	def DeleteSubscriber(self,doLog = True):
		raise NotImplementedError
	def StartSubscriber(self):
		raise NotImplementedError
	def StopSubscriber(self):
		raise NotImplementedError
	def SetDeadlineQos(self,timeoutMs):
		raise NotImplementedError


class CHelper(IHelper):
	def __init__(self):
		super(CHelper,self).__init__()
		self.listener = Listener()
		self.pub = None
		self.sub = None
		self.expectedPubId = -1
		self.data = None
	def HasPublisher(self):
		return self.pub is not None
	def HasSubscriber(self):
		return self.sub is not None


	def CreatePublisher(self,part,topicName):
		if self.pub is not None:
			print "Publisher already exist for topic " + self.pub.getTopic().getName()
		else:
			topic = part.createTopic(topicName);
			print "Created topic " + topic.name + " [" + topic.transport + "." + topic.domainAddress +"." + str(topic.port) + "]"
			self.pub = Publisher.Publisher(topic)
			self.pub.name = "PythonTest " + platform.system() + ("(%s)" % os.getpid())

	def DeletePublisher(self,doLog = True):
		if self.pub is not None:
			print "Deleting publisher for topic " + self.pub.topic.name
			del self.pub
		else:
			if doLog:
				print "Publisher must be created first!!"

	def StartPublisher(self):
		if self.pub is not None:
			self.pub.start();
		else:
			print "Publisher must be created first!!"

	def StopPublisher(self):
		if self.pub is not None:
			self.pub.stop();
		else:
			print "Publisher must be created first!!"

	def Write(self):
		if self.pub is not None:
			self.pub.write(self.data);
		else:
			print "Publisher must be created first!!"



	def CreateSubscriber(self,part,topicName):
		if self.sub is not None:
			print "Subscriber already exist for topic " + self.sub.getTopic().getName()
		else:
			topic = part.createTopic(topicName);
			print "Created topic " + topic.name + " [" + topic.transport + "." + topic.domainAddress +"." + str(topic.port) + "]"
			self.sub = Subscriber.Subscriber(topic)
			self.sub.addListener(self.listener)
			self.sub.start()
	def DeleteSubscriber(self):
		if (self.sub) {
			print "Deleting subscriber for topic " + self.sub.getTopic().getName()
			self.sub.stop();
			self.sub = None;
		else
			print "Subscriber must be created first!!"


	def StartSubscriber(self):
		if self.sub is not None:
			print "Starting subscriber for topic " + self.sub.getTopic().getName()
			self.sub.start();
		else:
			print "Subscriber must be created first!!"



class ItemInfo(object):
	def __init__(self,dom,top,typ):
		self.Domain = dom;
		self.TopicName = top
		self.TypeName = typ
		self.helper = None
		self.part = None
		self.selected = False
	def __str__(self):
		return ("P" if self.helper.HasPublisher() else " ") + ("S" if self.helper.HasPublisher() else " ") + ("*" if self.selected else " ") + self.Domain + "::" + self.TopicName


ItemInfoList = []
NumVessuvioBytes = 0
sendPeriod = 1000
FillerStr=""

def WriteToAllSelected():
	for info in ItemInfoList:
		if not info.selected:
			continue
		
		if info.TypeName == PizzaData.PizzaData.TypeName:
			info.helper.data.cheese = "Pizza from Python: " + str(WriteToAllSelected.Counter)

		if info.TypeName == PizzaData.VessuvioData.TypeName:
			info.helper.data.cheese = "Vessuvio from Python: " + str(WriteToAllSelected.Counter)
			info.helper.data.ham = FillerStr

		info.helper.Write()
		WriteToAllSelected.Counter+=1

WriteToAllSelected.Counter = 0

def menu():
	for i,k in enumerate(ItemInfoList):
		print ("\t %s " % i) + str(k)
	print ""
	print "\t PC    Create Publishers"
	print "\t PD    Delete Publishers"
	print "\t PS    Start Publishers"
	print "\t PT    Stop Publishers"
	print "\t SC    Create Subscriber"
	print "\t SD    Delete Subscriber"
	print "\t SS    Start Subscriber"
	print "\t ST    Stop Subscriber"
	print "\t L num Set num Vessuvio Bytes [%s]" % NumVessuvioBytes
	print "\t T ms  Set deadline timeout [ms]"
	print "\t V ms  Set send period [ms] [%s]" % sendPeriod
	print "\t A     Start/Stop periodical Write with set period"
	print "\t W     Write data"
	print "\t Q     Quite (minimize program output)"
	print "\t X     Exit program"



ItemInfoList.append(ItemInfo("PizzaDomain", "PizzaTopic", "pizza.PizzaData"))
ItemInfoList.append(ItemInfo("PizzaDomain", "VessuvioTopic", "pizza.VessuvioData"))

ItemInfoList.append(ItemInfo("PizzaDomain", "PizzaTopic2", "pizza.PizzaData"))
ItemInfoList.append(ItemInfo("PizzaDomain", "VessuvioTopic2", "pizza.VessuvioData"))

ItemInfoList.append(ItemInfo("PizzaDomain", "TcpPizzaTopic", "pizza.PizzaData"))
ItemInfoList.append(ItemInfo("PizzaDomain", "TcpVessuvioTopic", "pizza.VessuvioData"))

ItemInfoList.append(ItemInfo("PizzaDomain", "TcpPizzaTopic2", "pizza.PizzaData"))
ItemInfoList.append(ItemInfo("PizzaDomain", "TcpVessuvioTopic2", "pizza.VessuvioData"))

ItemInfoList.append(ItemInfo("PizzaDomain", "UdpPizzaTopic", "pizza.PizzaData"))
ItemInfoList.append(ItemInfo("PizzaDomain", "UdpVessuvioTopic", "pizza.VessuvioData"))

ItemInfoList.append(ItemInfo("OtherPizzaDomain", "OtherPizzaTopic", "pizza.PizzaData"))
ItemInfoList.append(ItemInfo("OtherPizzaDomain", "OtherVessuvioTopic", "pizza.VessuvioData"))

#ItemInfoList.append(ItemInfo("PizzaDomain", "ExtraAlltTopic", "pizza.special.ExtraAllt"))

ItemInfoList[0].selected = True

participant = Participant.Participant.getInstance("PizzaDomain", "PizzaDomain")
if participant == None:
	print "Failed to create Participant. Missing ops_config.xml ??"
	sys.exit(-1)
participant.addTypeSupport(PizzaData.pizza_factory())

otherParticipant = Participant.Participant.getInstance("OtherPizzaDomain", "OtherPizzaDomain")
if otherParticipant == None:
	print "Failed to create Participant. Missing ops_config.xml ??"
	sys.exit(-1)
otherParticipant.addTypeSupport(PizzaData.pizza_factory())




for info in ItemInfoList:
	if info.TypeName == PizzaData.PizzaData.TypeName:
		info.helper = CHelper()
		info.helper.data = PizzaData.PizzaData()
	elif info.TypeName == PizzaData.VessuvioData.TypeName:
		info.helper = CHelper()
		info.helper.data = PizzaData.VessuvioData()
	else:
		print "no matching typename for " + info.TypeName

	if info.Domain == "PizzaDomain":
		info.part = participant
	elif info.Domain == "OtherPizzaDomain":
		info.part = otherParticipant
	else:
		print "no matching domain for " + info.Domain

doExit = False
menu()

while not doExit:
	commands = re.split(" |\t", raw_input(" (? = menu) > ").upper())

	if commands[0].isdigit():
		index = int(commands[0])
		if 0 <= index < len(ItemInfoList):
			ItemInfoList[index].selected = not ItemInfoList[index].selected
		else:
			print "ERROR: Index to large. Max = %s" % (len(ItemInfoList)-1)

	elif (commands[0]=="?"):
		menu()
	elif (commands[0]=="X"):
		doExit = True


	elif (commands[0]=="PC"):
		for info in ItemInfoList:
			if info.selected:
				info.helper.CreatePublisher(info.part, info.TopicName)
	elif (commands[0]=="PD"):
		for info in ItemInfoList:
			if info.selected:
				info.helper.DeletePublisher()
	elif (commands[0]=="PS"):
		for info in ItemInfoList:
			if info.selected:
				info.helper.StartPublisher()
	elif (commands[0]=="PT"):
		for info in ItemInfoList:
			if info.selected:
				info.helper.StopPublisher()

	elif (commands[0]=="SC"):
		for info in ItemInfoList:
			if info.selected:
				info.helper.CreateSubscriber(info.part, info.TopicName)
	elif (commands[0]=="SD"):
		for info in ItemInfoList:
			if info.selected:
				info.helper.DeletePublisher()
	elif (commands[0]=="SS"):
		for info in ItemInfoList:
			if info.selected:
				info.helper.StartSubscriber()
	elif (commands[0]=="PT"):
		for info in ItemInfoList:
			if info.selected:
				info.helper.StopSubscriber()




	elif (commands[0]=="W"):
		WriteToAllSelected()
	elif (commands[0]=="L"):
		if commands[1].isdigit():
			num = int(commands[1])
			if num>=0:
				FillerStr=" "*num
				NumVessuvioBytes = num

	else:
		print "unknown command: " + commands[0]


