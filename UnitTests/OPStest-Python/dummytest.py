import time
import re
import sys
import os
import platform
import unittest


sys.path.append("/home/pane/slask/jali-ops/ops-development/Python")

from ops import Participant,Publisher,Subscriber,Print_Archiver

curDir = os.getcwd()
sys.path.append(curDir+"/../Pizzas")

import pizza
import pizza_special
import PizzaProjectTypeFactory


class minclass():
	TypeName = "pizza.special.Cheese"
	def __init__(self):
		self.name = ""
		self.age = 0.0

hej = minclass()
hej.name = "1111"
hej.age = 1.0

hej2 = minclass()
hej2.name = "2222"
hej2.age = 2.0

print hej2.name
print hej2.age


