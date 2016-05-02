import time
import re
import sys
import os
import platform
import unittest

sys.path.append("/home/pane/slask/jali-ops/ops-development/Python")
from ops import Participant,Publisher,Subscriber,Print_Archiver, OPS_Archiver, Archiver
from ops.opsTypes import OPS_Object

curDir = os.getcwd()
sys.path.append(curDir+"/../Pizzas")

import pizza
import pizza_special
import PizzaProjectTypeFactory
import InitData

delta = 5

class Test_Serialization_Deserialization(unittest.TestCase):
	def test_extraAllt(self):
		
		m_factory = PizzaProjectTypeFactory.PizzaProjectTypeFactory()
		
		extraAllt = InitData.initExtraAlltNormal()

		m_OPS_archiver_out = OPS_Archiver.OPS_Archiver_Out(10000) #buffer size

		#serializing
		m_OPS_archiver_out.Bool("extraCheese", extraAllt.extraCheese)
		m_OPS_archiver_out.Int8("nrOfMushRooms", extraAllt.nrOfMushRooms)
		m_OPS_archiver_out.Int32("meetQuality", extraAllt.meetQuality)
		m_OPS_archiver_out.Int64("timestamp", extraAllt.timestamp)
		m_OPS_archiver_out.Float32("timeBakedHours", extraAllt.timeBakedHours)
		m_OPS_archiver_out.Float64("timeBakedSeconds", extraAllt.timeBakedSeconds)
		m_OPS_archiver_out.String("description", extraAllt.description)
		m_OPS_archiver_out.Ops("cheese_", extraAllt.cheese_)
		m_OPS_archiver_out.BoolVector("bools", extraAllt.bools)
		m_OPS_archiver_out.Int8Vector("bytes", extraAllt.bytes)
		m_OPS_archiver_out.Int32Vector("ints", extraAllt.ints)
		m_OPS_archiver_out.Int64Vector("longs", extraAllt.longs)
		m_OPS_archiver_out.Float32Vector("floats", extraAllt.floats)
		m_OPS_archiver_out.Float64Vector("doubles", extraAllt.doubles)
		m_OPS_archiver_out.StringVector("strings", extraAllt.strings)
		m_OPS_archiver_out.OpsVector("cheeses", extraAllt.cheeses)
		
		#unpacking data
		m_OPS_archiver_in = OPS_Archiver.OPS_Archiver_In(m_factory,m_OPS_archiver_out.buffer)
		
		unpacked_extraAllt = pizza_special.ExtraAllt()
		unpacked_extraAllt.extraCheese = m_OPS_archiver_in.Bool("extraCheese", m_OPS_archiver_out.buffer)
		unpacked_extraAllt.nrOfMushRooms = m_OPS_archiver_in.Int8("nrOfMushRooms", m_OPS_archiver_out.buffer)
		unpacked_extraAllt.meetQuality = m_OPS_archiver_in.Int32("meetQuality", m_OPS_archiver_out.buffer)
		unpacked_extraAllt.timestamp = m_OPS_archiver_in.Int64("timestamp", m_OPS_archiver_out.buffer)
		unpacked_extraAllt.timeBakedHours = m_OPS_archiver_in.Float32("timeBakedHours", m_OPS_archiver_out.buffer)
		unpacked_extraAllt.timeBakedSeconds = m_OPS_archiver_in.Float64("timeBakedSeconds", m_OPS_archiver_out.buffer)
		unpacked_extraAllt.description = m_OPS_archiver_in.String("description", m_OPS_archiver_out.buffer)
		unpacked_extraAllt.cheese_ = m_OPS_archiver_in.Ops("cheese_", m_OPS_archiver_out.buffer)
		
		
		#unpacling vectors
		unpacked_bools = []
		unpacked_bytes = []
		unpacked_ints  = []
		unpacked_longs = []
		unpacked_floats = []
		unpacked_doubles = []
		unpacked_strings = []
		unpacked_cheeses = []
		
		
		m_OPS_archiver_in.BoolVector("bools", unpacked_bools)
		m_OPS_archiver_in.Int8Vector("bytes", unpacked_bytes)
		m_OPS_archiver_in.Int32Vector("ints", unpacked_ints)
		m_OPS_archiver_in.Int64Vector("longs", unpacked_longs)
		m_OPS_archiver_in.Float32Vector("floats", unpacked_floats)
		m_OPS_archiver_in.Float64Vector("doubles", unpacked_doubles)
		m_OPS_archiver_in.StringVector("strings", unpacked_strings)
		m_OPS_archiver_in.OpsVector("cheeses", unpacked_cheeses)
		
		unpacked_extraAllt.bools = unpacked_bools
		unpacked_extraAllt.bytes = unpacked_bytes
		unpacked_extraAllt.ints = unpacked_ints
		unpacked_extraAllt.floats = unpacked_floats
		unpacked_extraAllt.longs = unpacked_longs
		unpacked_extraAllt.doubles = unpacked_doubles
		unpacked_extraAllt.strings = unpacked_strings
		unpacked_extraAllt.cheeses = unpacked_cheeses
		
		
		#test data
		self.assertEqual(unpacked_extraAllt.extraCheese 	, True )
		self.assertEqual(unpacked_extraAllt.nrOfMushRooms	, 14 )
		self.assertEqual(unpacked_extraAllt.meetQuality	   	, 9 )
		self.assertAlmostEqual(unpacked_extraAllt.timeBakedHours 	, 123.4, delta )
		self.assertEqual(unpacked_extraAllt.timeBakedSeconds, 53.4 )
		self.assertEqual(unpacked_extraAllt.description	   	, "Pizza with extra allt" )
		self.assertEqual(unpacked_extraAllt.cheese_.age		, 12.0 )
		self.assertEqual(unpacked_extraAllt.cheese_.name	, "gorgonzola" )
		
		self.assertEqual(len(unpacked_extraAllt.bools)		, 6)
		
		self.assertEqual(len(unpacked_extraAllt.bytes)		, 6)
		self.assertEqual(len(unpacked_extraAllt.ints) 		, 5)
		self.assertEqual(len(unpacked_extraAllt.floats) 	, 5)
		self.assertEqual(len(unpacked_extraAllt.doubles) 	, 5)
		self.assertEqual(len(unpacked_extraAllt.longs) 		, 5)
		self.assertEqual(len(unpacked_extraAllt.strings) 	, 6)
		self.assertEqual(len(unpacked_extraAllt.cheeses) 	, 4)
		
		self.assertEqual(unpacked_extraAllt.bools   		, [True, False, True, False, True, False] )
		self.assertEqual(unpacked_extraAllt.bytes   		, [-64, -32, -16, 15, 31, 63] )
		self.assertEqual(unpacked_extraAllt.ints    		, [0, 123, -523, 1000, -5000] )
		self.assertEqual(unpacked_extraAllt.longs   		, [0, 123, -523, 1000, -5000] )
		self.assertEqual(unpacked_extraAllt.floats  		, [0.0, 123.0, -523.0, 1000.0, -5000.0] )
		self.assertEqual(unpacked_extraAllt.doubles 		, [0.0, 123.0, -523.0, 1000.0, -5000.0] )
		self.assertEqual(unpacked_extraAllt.strings 		, ["extra", "allt", "er", "den", "basta", "pizzan"] )
		self.assertEqual(unpacked_extraAllt.cheeses[0].age 	, 1 )
		self.assertEqual(unpacked_extraAllt.cheeses[0].name , "ost1" )
		self.assertEqual(unpacked_extraAllt.cheeses[1].age 	, 2 )
		self.assertEqual(unpacked_extraAllt.cheeses[1].name , "ost2" )
		self.assertEqual(unpacked_extraAllt.cheeses[2].age 	, 3 )
		self.assertEqual(unpacked_extraAllt.cheeses[2].name , "ost3" )
		self.assertEqual(unpacked_extraAllt.cheeses[3].age 	, 4 )
		self.assertEqual(unpacked_extraAllt.cheeses[3].name , "ost4" )
		
	def test_vessuvio(self):
		m_factory = PizzaProjectTypeFactory.PizzaProjectTypeFactory()
		
		vessuvioData = InitData.initVessuvioData()

		#serialize
		m_OPS_archiver_out = OPS_Archiver.OPS_Archiver_Out(200) #buffer size
		
		m_OPS_archiver_out.String("ham", vessuvioData.ham)
		m_OPS_archiver_out.String("cheese", vessuvioData.cheese)
		m_OPS_archiver_out.String("tomatoSauce", vessuvioData.tomatoSauce)
		
		#unpack
		m_OPS_archiver_in = OPS_Archiver.OPS_Archiver_In(m_factory,m_OPS_archiver_out.buffer)
		
		unpacked_vessuvioData = pizza.VessuvioData()
		unpacked_vessuvioData.ham = m_OPS_archiver_in.String("ham", m_OPS_archiver_out.buffer)
		unpacked_vessuvioData.cheese = m_OPS_archiver_in.String("cheese", m_OPS_archiver_out.buffer)
		unpacked_vessuvioData.tomatoSauce = m_OPS_archiver_in.String("tomatoSauce", m_OPS_archiver_out.buffer)
		
		#test data
		self.assertEqual(unpacked_vessuvioData.ham, "my ham")
		self.assertEqual(unpacked_vessuvioData.cheese, "my cheese")
		self.assertEqual(unpacked_vessuvioData.tomatoSauce, "my tomatosauce")
		
		

if __name__ == '__main__':
	suite = unittest.TestLoader().loadTestsFromTestCase(Test_Serialization_Deserialization)
	unittest.TextTestRunner(verbosity=2).run(suite)



