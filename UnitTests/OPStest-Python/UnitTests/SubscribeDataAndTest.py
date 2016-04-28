import time
import re
import sys
import os
import platform
import unittest


sys.path.append("/home/pane/slask/jali-ops/ops-development/Python")
sys.path.append("/home/pane/slask/pytest/pytest-2.9.1")

from ops import Participant,Publisher,Subscriber,Print_Archiver

curDir = os.getcwd()
sys.path.append(curDir+"/../Pizzas")

import pizza
import pizza_special
import PizzaProjectTypeFactory
#import pytest

#global variables
receivedData = []
delta = 5


#test class normal unit test

class TestExtraAllt(unittest.TestCase):
	def test_normal(self):
		print ""
		for i in range(0,len(receivedData)-1):
			extraAllt = receivedData[i]
			if i == 0 or (i >= 2 and i < 12):
				self.assertEqual(len(extraAllt.bools), 6)
				self.assertEqual(len(extraAllt.bytes), 6)
				self.assertEqual(len(extraAllt.ints) , 5)
				self.assertEqual(len(extraAllt.floats) , 5)
				self.assertEqual(len(extraAllt.doubles) , 5)
				self.assertEqual(len(extraAllt.longs) , 5)
				self.assertEqual(len(extraAllt.strings) , 6)
				self.assertEqual(len(extraAllt.cheeses) , 4)
				
				self.assertEqual( extraAllt.extraCheese 	, True )
				self.assertEqual( extraAllt.nrOfMushRooms	, 14 )
				self.assertEqual( extraAllt.meetQuality	   	, 9 )
				self.assertAlmostEqual( extraAllt.timeBakedHours 	, 123.4, delta )
				self.assertEqual( extraAllt.timeBakedSeconds, 53.4 )
				self.assertEqual( extraAllt.description	   	, "Pizza with extra allt" )
				self.assertEqual( extraAllt.cheese_.age		, 12.0 )
				self.assertEqual( extraAllt.cheese_.name	, "gorgonzola" )
				
				
				self.assertEqual( extraAllt.bools   		, [True, False, True, False, True, False] )
				self.assertEqual( extraAllt.bytes   		, [-64, -32, -16, 15, 31, 63] )
				self.assertEqual( extraAllt.ints    		, [0, 123, -523, 1000, -5000] )
				self.assertEqual( extraAllt.longs   		, [0, 123, -523, 1000, -5000] )
				self.assertEqual( extraAllt.floats  		, [0.0, 123.0, -523.0, 1000.0, -5000.0] )
				self.assertEqual( extraAllt.doubles 		, [0.0, 123.0, -523.0, 1000.0, -5000.0] )
				self.assertEqual( extraAllt.strings 		, ["extra", "allt", "er", "den", "basta", "pizzan"] )
				self.assertEqual( extraAllt.cheeses[0].age 	, 1 )
				self.assertEqual( extraAllt.cheeses[0].name , "ost1" )
				self.assertEqual( extraAllt.cheeses[1].age 	, 2 )
				self.assertEqual( extraAllt.cheeses[1].name , "ost2" )
				self.assertEqual( extraAllt.cheeses[2].age 	, 3 )
				self.assertEqual( extraAllt.cheeses[2].name , "ost3" )
				self.assertEqual( extraAllt.cheeses[3].age 	, 4 )
				self.assertEqual( extraAllt.cheeses[3].name , "ost4" )
			else:
				self.assertEqual( extraAllt.extraCheese 	, True )
				self.assertEqual( extraAllt.nrOfMushRooms	, 14 )
				self.assertEqual( extraAllt.meetQuality	   	, 9 )
				self.assertAlmostEqual( extraAllt.timeBakedHours 	, 123.4, delta )
				self.assertEqual( extraAllt.timeBakedSeconds, 53.4 )
				self.assertEqual( extraAllt.description	   	, "Pizza with extra allt" )
				self.assertEqual( extraAllt.cheese_.age		, 12.0 )
				self.assertEqual( extraAllt.cheese_.name	, "gorgonzola" )
				
				self.assertEqual(len(extraAllt.bools), 200)
				self.assertEqual(len(extraAllt.bytes), 200)
				self.assertEqual(len(extraAllt.ints) , 200)
				self.assertEqual(len(extraAllt.floats) , 200)
				self.assertEqual(len(extraAllt.doubles) , 200)
				self.assertEqual(len(extraAllt.longs) , 200)
				self.assertEqual(len(extraAllt.strings) , 200)
				self.assertEqual(len(extraAllt.cheeses) , 200)
				for j in range(0,200):
					if j <=	 100:
						self.assertEqual(extraAllt.bools[j] , True )
						self.assertEqual(extraAllt.bytes[j] , -42 )
						self.assertEqual(extraAllt.ints[j] , 5 )
						self.assertEqual(extraAllt.floats[j] , 15.0 )
						self.assertEqual(extraAllt.doubles[j] , 25.0 )
						self.assertEqual(extraAllt.longs[j] , 35 )
						self.assertEqual(extraAllt.strings[j] , "hejsan" )
						self.assertEqual(extraAllt.cheeses[j].age , 3 )
						self.assertEqual(extraAllt.cheeses[j].name , "ecklig ost" )
					else:
						self.assertEqual(extraAllt.bools[j] , False )
						self.assertEqual(extraAllt.bytes[j] , 42 )
						self.assertEqual(extraAllt.ints[j] , 10 )
						self.assertEqual(extraAllt.floats[j] , 20.0 )
						self.assertEqual(extraAllt.doubles[j] , 30.0 )
						self.assertEqual(extraAllt.longs[j] , 40 )
						self.assertEqual(extraAllt.strings[j] , "hoppsan" )
						self.assertEqual(extraAllt.cheeses[j].age , 6 )
						self.assertEqual(extraAllt.cheeses[j].name , "god ost" )


										
					

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


participant = Participant.Participant.getInstance("PizzaDomain", "PizzaDomain", "../ops_config.xml")
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


if __name__ == '__main__':
	suite = unittest.TestLoader().loadTestsFromTestCase(TestExtraAllt)
	unittest.TextTestRunner(verbosity=2).run(suite)



