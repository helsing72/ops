#!/bin/bash

if [[ 0 -eq 1 ]]
then
	echo "kommer in hit?"
	if [[ $GOOGLETEST_HOME != /sw/tools/google-test/1.7 ]] 
	then
		export GOOGLETEST_HOME=/sw/tools/google-test/1.7
	fi

	if [[ $GCOVR_HOME != /sw/tools/gcovr/3.2 ]] 
	then
		export GCOVR_HOME=/sw/tools/gcovr/3.2
	fi

	if [[ $BOOST_HOME != /sw/tools/boost/1.60 ]] 
	then
		export BOOST_HOME=/sw/tools/boost/1.60
	fi

	echo "echoing PAATH $PATH "

	if [[ $PATH != *'/sw/tools/gcc/5.3.0'* ]] 
	then
		echo "exporting path"
		export PATH=/sw/tools/gcc/5.3.0:${PATH}
		export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:/sw/tools/gcc/5.3.0/lib64:/sw/tools/gcc/5.3.0/lib
	fi
fi

echo "echoing PAATH $PATH "

#cd /lkp/nlt/dev/users/pane/nlt/OPS4/ops4
cd ../..
JENK_DIR="/tmp/jenkins/workspace/OPS_PizzaTest"
echo "jag ar i mapp"
pwd
build.debug/UnitTests/OPStest/UnitTests/test-serialize-and-deserialize --gtest_output="xml:$JENK_DIR/Unittest-serialize-and-deserialize_result.xml"

build.debug/UnitTests/OPStest/UnitTests/test-subscribe  	--gtest_output="xml:$JENK_DIR/Unittest-publish-and-subscribe_result.xml"     &
build.debug/UnitTests/OPStest/UnitTests/test-subscribeTCP	--gtest_output="xml:$JENK_DIR/Unittest-publish-and-subscribe_result_tcp.xml" &
build.debug/UnitTests/OPStest/UnitTests/test-subscribeUDP  	--gtest_output="xml:$JENK_DIR/Unittest-publish-and-subscribe_result_udp.xml" &
sleep 1
build.debug/UnitTests/OPStest/src/publishBin

mkdir -p Unit_test_results/Coverage
gcovr --root . -b --xml --xml-pretty -o $JENK_DIR/coverage_result_OPS_jenkins.xml


bash
	cppcheck --enable=all --inconclusive --xml --xml-version=2 Cpp/source Cpp/include 2> $JENK_DIR/cppcheck_result_OPS_jenkins.xml
exit


