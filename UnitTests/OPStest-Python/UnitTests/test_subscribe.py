import time
import re
import sys
import os
import platform

from ops import Participant,Publisher,Subscriber,Print_Archiver


import pizza
import pizza_special
import PizzaProjectTypeFactory

#global variables
receivedData = []
delta = 0.00001


#test class normal unit test


def test_normal():
	assert(len(receivedData) == 22)
	for i in range(0,len(receivedData)-1):
		extraAllt = receivedData[i]
		if i == 0 or (i >= 2 and i < 12):
			assert(len(extraAllt.bools)== 6)
			assert(len(extraAllt.bytes)== 6)
			assert(len(extraAllt.ints) == 5)
			assert(len(extraAllt.floats) == 5)
			assert(len(extraAllt.doubles) == 5)
			assert(len(extraAllt.longs) == 5)
			assert(len(extraAllt.strings) == 6)
			assert(len(extraAllt.cheeses) == 4)
			
			assert( extraAllt.extraCheese 		== True )
			assert( extraAllt.nrOfMushRooms		== 14 )
			assert( extraAllt.meetQuality	   	== 9 )
			assert( abs(extraAllt.timeBakedHours - 123.4) < delta )
			assert( extraAllt.timeBakedSeconds	== 53.4 )
			assert( extraAllt.description	   	== "Pizza with extra allt" )
			assert( extraAllt.cheese_.age		== 12.0 )
			assert( extraAllt.cheese_.name	== "gorgonzola" )
			
			
			assert( extraAllt.bools   		== [True, False, True, False, True, False] )
			assert( extraAllt.bytes   		== [-64, -32, -16, 15, 31, 63] )
			assert( extraAllt.ints    		== [0, 123, -523, 1000, -5000] )
			assert( extraAllt.longs   		== [0, 123, -523, 1000, -5000] )
			assert( extraAllt.floats  		== [0.0, 123.0, -523.0, 1000.0, -5000.0] )
			assert( extraAllt.doubles 		== [0.0, 123.0, -523.0, 1000.0, -5000.0] )
			assert( extraAllt.strings 		== ["extra", "allt", "er", "den", "basta", "pizzan"] )
			assert( extraAllt.cheeses[0].age 	== 1 )
			assert( extraAllt.cheeses[0].name == "ost1" )
			assert( extraAllt.cheeses[1].age 	== 2 )
			assert( extraAllt.cheeses[1].name == "ost2" )
			assert( extraAllt.cheeses[2].age 	== 3 )
			assert( extraAllt.cheeses[2].name == "ost3" )
			assert( extraAllt.cheeses[3].age 	== 4 )
			assert( extraAllt.cheeses[3].name == "ost4" )
		else:
			assert( extraAllt.extraCheese 	== True )
			assert( extraAllt.nrOfMushRooms	== 14 )
			assert( extraAllt.meetQuality	   	== 9 )
			assert( abs(extraAllt.timeBakedHours - 123.4) < delta)
			assert( extraAllt.timeBakedSeconds== 53.4 )
			assert( extraAllt.description	   	== "Pizza with extra allt" )
			assert( extraAllt.cheese_.age		== 12.0 )
			assert( extraAllt.cheese_.name	== "gorgonzola" )
			
			assert(len(extraAllt.bools)		== 200)
			assert(len(extraAllt.bytes) 	== 200)
			assert(len(extraAllt.ints) 		== 200)
			assert(len(extraAllt.floats) 	== 200)
			assert(len(extraAllt.doubles) 	== 200)
			assert(len(extraAllt.longs)		== 200)
			assert(len(extraAllt.strings) 	== 200)
			assert(len(extraAllt.cheeses) 	== 200)
			for j in range(0,200):
				if j <=	 100:
					assert(extraAllt.bools[j] 		== True )
					assert(extraAllt.bytes[j] 		== -42 )
					assert(extraAllt.ints[j] 		== 5 )
					assert(extraAllt.floats[j] 		== 15.0 )
					assert(extraAllt.doubles[j] 	== 25.0 )
					assert(extraAllt.longs[j] 		== 35 )
					assert(extraAllt.strings[j] 	== "hejsan" )
					assert(extraAllt.cheeses[j].age == 3 )
					assert(extraAllt.cheeses[j].name== "ecklig ost" )
				else:
					assert(extraAllt.bools[j] == False )
					assert(extraAllt.bytes[j] == 42 )
					assert(extraAllt.ints[j] == 10 )
					assert(extraAllt.floats[j] == 20.0 )
					assert(extraAllt.doubles[j] == 30.0 )
					assert(extraAllt.longs[j] == 40 )
					assert(extraAllt.strings[j] == "hoppsan" )
					assert(extraAllt.cheeses[j].age == 6 )
					assert(extraAllt.cheeses[j].name == "god ost" )

										
					

def onExtraAllt(sub,mess):
	addr,port = map(str,mess.getSource())
	data=mess.data
	receivedData.append(data)



class CHelper(object):
	def __init__(self):
		super(CHelper,self).__init__()
		self.callback = None
		self.pub = None
		self.sub = None
		self.expectedPubId = -1
		self.data = None


	def CreateSubscriber(self,part,topicName):
		if self.sub is not None:
			print "Subscriber already exist for topic " + self.sub.getTopic().getName()
		else:
			topic = part.createTopic(topicName);
			print "Created topic " + topic.name + " [" + topic.transport + "." + topic.domainAddress +"." + str(topic.port) + "]"
			self.sub = Subscriber.Subscriber(topic)
			#self.sub.addCallback(self.listener.onNewData)
			if self.callback is not None:
				self.sub.addCallback(self.callback)
			self.sub.start()
			
	def DeleteSubscriber(self,doLog = True):
		print "Deleting subscriber for topic " + self.sub.getTopic().getName()
		self.sub.stop();
		self.sub = None;

	def StartSubscriber(self):
		self.sub.start();
	
	def StopSubscriber(self):
		self.sub.stop()


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

ItemInfoList.append(ItemInfo("PizzaDomain", "ExtraAlltTopic", "pizza.special.ExtraAllt"))

participant = Participant.Participant.getInstance("PizzaDomain", "PizzaDomain", "ops_config.xml")
if participant == None:
	print "Failed to create Participant. Missing ops_config.xml ??"
	sys.exit(-1)
participant.addTypeSupport(PizzaProjectTypeFactory.PizzaProjectTypeFactory())


info = ItemInfoList[0]

#create subsrcriber
info.helper = CHelper()
info.helper.data = pizza_special.ExtraAllt()
info.helper.callback = onExtraAllt
info.part = participant

info.helper.CreateSubscriber(info.part, info.TopicName)
info.helper.StartSubscriber()

#wait until data is collected
time.sleep(11)

#delete subscriber
#info.helper.StopSubscriber()
#info.helper.DeleteSubscriber()

