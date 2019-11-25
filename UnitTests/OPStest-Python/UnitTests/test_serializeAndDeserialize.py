import time
import re
import sys
import os
import platform


from ops import Participant,Publisher,Subscriber,Print_Archiver, OPS_Archiver, Archiver
from ops.opsTypes import OPS_Object

import pizza
import pizza_special
import PizzaProjectTypeFactory
import InitData

#whats a good value?
delta = 0.00001

def test_extraAllt():
	print("testing extra allt")

	m_factory = PizzaProjectTypeFactory.PizzaProjectTypeFactory()

	#initialize data to pack / unpack
	extraAllt = InitData.initExtraAlltNormal()

	m_OPS_archiver_out = OPS_Archiver.OPS_Archiver_Out(10000, False) #buffer size

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

	unpacked_extraAllt 					= pizza_special.ExtraAllt()
	unpacked_extraAllt.extraCheese 		= m_OPS_archiver_in.Bool("extraCheese", m_OPS_archiver_out.buffer)
	unpacked_extraAllt.nrOfMushRooms 	= m_OPS_archiver_in.Int8("nrOfMushRooms", m_OPS_archiver_out.buffer)
	unpacked_extraAllt.meetQuality 		= m_OPS_archiver_in.Int32("meetQuality", m_OPS_archiver_out.buffer)
	unpacked_extraAllt.timestamp 		= m_OPS_archiver_in.Int64("timestamp", m_OPS_archiver_out.buffer)
	unpacked_extraAllt.timeBakedHours 	= m_OPS_archiver_in.Float32("timeBakedHours", m_OPS_archiver_out.buffer)
	unpacked_extraAllt.timeBakedSeconds = m_OPS_archiver_in.Float64("timeBakedSeconds", m_OPS_archiver_out.buffer)
	unpacked_extraAllt.description 		= m_OPS_archiver_in.String("description", m_OPS_archiver_out.buffer)
	unpacked_extraAllt.cheese_ 			= m_OPS_archiver_in.Ops("cheese_", m_OPS_archiver_out.buffer)


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

	unpacked_extraAllt.bools 	= unpacked_bools
	unpacked_extraAllt.bytes 	= unpacked_bytes
	unpacked_extraAllt.ints 	= unpacked_ints
	unpacked_extraAllt.floats 	= unpacked_floats
	unpacked_extraAllt.longs 	= unpacked_longs
	unpacked_extraAllt.doubles 	= unpacked_doubles
	unpacked_extraAllt.strings 	= unpacked_strings
	unpacked_extraAllt.cheeses 	= unpacked_cheeses


	#test data
	assert(unpacked_extraAllt.extraCheese 		== True )
	assert(unpacked_extraAllt.nrOfMushRooms		== 14 )
	assert(unpacked_extraAllt.meetQuality		== 9 )
	assert(abs(unpacked_extraAllt.timeBakedHours -123.4) < delta )
	assert(unpacked_extraAllt.timeBakedSeconds	== 53.4 )
	assert(unpacked_extraAllt.description		== "Pizza with extra allt" )
	assert(unpacked_extraAllt.cheese_.age		== 12.0 )
	assert(unpacked_extraAllt.cheese_.name		== "gorgonzola" )
	assert(len(unpacked_extraAllt.bools)		== 6)
	assert(len(unpacked_extraAllt.bytes)		== 6)
	assert(len(unpacked_extraAllt.ints) 		== 5)
	assert(len(unpacked_extraAllt.floats) 		== 5)
	assert(len(unpacked_extraAllt.doubles) 		== 5)
	assert(len(unpacked_extraAllt.longs) 		== 5)
	assert(len(unpacked_extraAllt.strings) 		== 6)
	assert(len(unpacked_extraAllt.cheeses) 		== 4)
	assert(unpacked_extraAllt.bools  	 		== [True, False, True, False, True, False] )
	assert(unpacked_extraAllt.bytes   			== [-64, -32, -16, 15, 31, 63] )
	assert(unpacked_extraAllt.ints    			== [0, 123, -523, 1000, -5000] )
	assert(unpacked_extraAllt.longs   			== [0, 123, -523, 1000, -5000] )
	assert(unpacked_extraAllt.floats  			== [0.0, 123.0, -523.0, 1000.0, -5000.0] )
	assert(unpacked_extraAllt.doubles 			== [0.0, 123.0, -523.0, 1000.0, -5000.0] )
	assert(unpacked_extraAllt.strings 			== ["extra", "allt", "er", "den", "basta", "pizzan"] )
	assert(unpacked_extraAllt.cheeses[0].age 	== 1 )
	assert(unpacked_extraAllt.cheeses[0].name 	== "ost1" )
	assert(unpacked_extraAllt.cheeses[1].age 	== 2 )
	assert(unpacked_extraAllt.cheeses[1].name 	== "ost2" )
	assert(unpacked_extraAllt.cheeses[2].age 	== 3 )
	assert(unpacked_extraAllt.cheeses[2].name 	== "ost3" )
	assert(unpacked_extraAllt.cheeses[3].age 	== 4 )
	assert(unpacked_extraAllt.cheeses[3].name 	== "ost4" )



def test_vessuvio():
	m_factory = PizzaProjectTypeFactory.PizzaProjectTypeFactory()
	print("testing vessuvio")
	vessuvioData = InitData.initVessuvioData()

	#serialize
	m_OPS_archiver_out = OPS_Archiver.OPS_Archiver_Out(200, False) #buffer size
	m_OPS_archiver_out.String("ham" , vessuvioData.ham)
	m_OPS_archiver_out.String("cheese" , vessuvioData.cheese)
	m_OPS_archiver_out.String("tomatoSauce" , vessuvioData.tomatoSauce)

	#unpack
	m_OPS_archiver_in = OPS_Archiver.OPS_Archiver_In(m_factory,m_OPS_archiver_out.buffer)

	unpacked_vessuvioData = pizza.VessuvioData()
	unpacked_vessuvioData.ham = m_OPS_archiver_in.String("ham" , m_OPS_archiver_out.buffer)
	unpacked_vessuvioData.cheese = m_OPS_archiver_in.String("cheese" , m_OPS_archiver_out.buffer)
	unpacked_vessuvioData.tomatoSauce = m_OPS_archiver_in.String("tomatoSauce" , m_OPS_archiver_out.buffer)

	#test data
	assert(unpacked_vessuvioData.ham == "my ham")
	assert(unpacked_vessuvioData.cheese == "my cheese")
	assert(unpacked_vessuvioData.tomatoSauce == "my tomatosauce")
