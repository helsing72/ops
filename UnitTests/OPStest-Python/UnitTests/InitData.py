import os,sys

curDir = os.getcwd()
sys.path.append(curDir+"/../Pizzas")
import pizza
import pizza_special

def initExtraAlltNormal():
	m_extraAllt = pizza_special.ExtraAllt()
	m_extraAllt.extraCheese 		= True
	m_extraAllt.nrOfMushRooms		= 14
	m_extraAllt.meetQuality	   		= 9
	m_extraAllt.timeBakedHours 		= 123.4
	m_extraAllt.timeBakedSeconds	= 53.4
	m_extraAllt.description	   		= "Pizza with extra allt"
	
	m_extraAllt.bools   = [True, False, True, False, True, False]
	m_extraAllt.bytes   = [-64, -32, -16, 15, 31, 63]
	m_extraAllt.ints    = [0, 123, -523, 1000, -5000]
	m_extraAllt.longs   = [0, 123, -523, 1000, -5000]
	m_extraAllt.floats  = [0.0, 123.0, -523.0, 1000.0, -5000.0]
	m_extraAllt.doubles = [0.0, 123.0, -523.0, 1000.0, -5000.0]
	m_extraAllt.strings = ["extra", "allt", "er", "den", "basta", "pizzan"]
	
	m_cheese = pizza_special.Cheese()
	m_cheese.name = "gorgonzola"
	m_cheese.age  = 12.0
	m_extraAllt.cheese_ = m_cheese

	m_listCheese1 = pizza_special.Cheese()
	m_listCheese1.name = "ost1"
	m_listCheese1.age  = 1.0

	m_listCheese2 = pizza_special.Cheese()
	m_listCheese2.name = "ost2"
	m_listCheese2.age  = 2.0

	m_listCheese3 = pizza_special.Cheese()
	m_listCheese3.name = "ost3"
	m_listCheese3.age  = 3.0

	m_listCheese4 = pizza_special.Cheese()
	m_listCheese4.name = "ost4"
	m_listCheese4.age  = 4.0
	
	m_extraAllt.cheeses = [m_listCheese1, m_listCheese2, m_listCheese3, m_listCheese4]
	
	return m_extraAllt


def initExtraAlltLarge():
	m_extraAllt = pizza_special.ExtraAllt()
	vecSize = 200
	halfVecsize = vecSize/2


	m_extraAllt.extraCheese 		= True
	m_extraAllt.nrOfMushRooms		= 14
	m_extraAllt.meetQuality	   		= 9
	m_extraAllt.timeBakedHours 		= 123.4
	m_extraAllt.timeBakedSeconds	= 53.4
	m_extraAllt.description	   		= "Pizza with extra allt"
	m_cheese = pizza_special.Cheese()
	m_cheese.name = "gorgonzola"
	m_cheese.age  = 12.0
	m_extraAllt.cheese_ = m_cheese


	boolVal = True
	byteVal = -42
	intVal = 5
	floatVal = 15.0
	doubleVal = 25.0
	longVal = 35
	stringVal = "hejsan"
	cheeseVal = pizza_special.Cheese()
	cheeseVal.age = 3
	cheeseVal.name = "ecklig ost"



	for i in range(0,vecSize):
		m_extraAllt.bools.append(boolVal)
		m_extraAllt.bytes.append(byteVal)
		m_extraAllt.ints.append(intVal)
		m_extraAllt.floats.append(floatVal)
		m_extraAllt.doubles.append(doubleVal)
		m_extraAllt.longs.append(longVal)
		m_extraAllt.strings.append(stringVal)
		m_extraAllt.cheeses.append(cheeseVal)

		if i == halfVecsize:
			boolVal        	= False
			byteVal 		= 42
			intVal 			= 10
			floatVal 		= 20.0
			doubleVal 		= 30.0
			longVal 	   	= 40
			stringVal 		= "hoppsan"
			cheeseVal = pizza_special.Cheese()
			cheeseVal.age 	= 6
			cheeseVal.name	= "god ost"
			
	return m_extraAllt
