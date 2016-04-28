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


def test_vessuvio_ser_deser_obj(vessData):
	"""
	ops::MemoryMap map(1, 10000);
	ops::ByteBuffer buf(&map);

	ops::SerializableInheritingTypeFactory fact;
	fact.add(new PizzaProject::PizzaProjectTypeFactory());

	ops::OPSArchiverOut out(&buf);
	ops::OPSArchiverIn  in(&buf, &fact);

	out.inout(std::string("data"), &vessuvio);

	EXPECT_EQ(buf.GetSize(), 89);

	buf.Reset();

	EXPECT_EQ(buf.GetSize(), 0);

	recreatedVessuvio = dynamic_cast<pizza::VessuvioData*>(in.inout(std::string("data"), recreatedVessuvio));

	EXPECT_TRUE(recreatedVessuvio) << "failed creating Vessuvio object";

	test::testVessuvio(*recreatedVessuvio);

	EXPECT_EQ((int) buf.GetSize(), 89);
	"""



def test_vessuvio_ser_deser_mes(vessData):


def test_extraallt_ser_deser_obj(extraAllt):



def test_extraallt_ser_deser_mes(extraAllt):





