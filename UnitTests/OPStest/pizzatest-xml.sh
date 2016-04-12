#!/bin/bash

#cd /lkp/nlt/dev/users/pane/nlt/OPS4/ops4
cd ../..
JENK_DIR="/tmp/jenkins/workspace/OPS_PizzaTest"
UNIT_DIR="Unit_test_results/"
build.debug/UnitTests/OPStest/UnitTests/test-serialize-and-deserialize --gtest_output="xml:$JENK_DIR/Unittest-serialize-and-deserialize_result.xml"

build.debug/UnitTests/OPStest/UnitTests/test-subscribe  	--gtest_output="xml:$JENK_DIR/Unittest-publish-and-subscribe_result.xml"     &
build.debug/UnitTests/OPStest/UnitTests/test-subscribeTCP	--gtest_output="xml:$JENK_DIR/Unittest-publish-and-subscribe_result_tcp.xml" &
build.debug/UnitTests/OPStest/UnitTests/test-subscribeUDP  	--gtest_output="xml:$JENK_DIR/Unittest-publish-and-subscribe_result_udp.xml" &

#build.debug/UnitTests/OPStest/UnitTests/test-subscribe  	--gtest_output="xml:$UNIT_DIR/Unittest-publish-and-subscribe_result.xml"     &
#build.debug/UnitTests/OPStest/UnitTests/test-subscribeTCP	--gtest_output="xml:$UNIT_DIR/Unittest-publish-and-subscribe_result_tcp.xml" &
#build.debug/UnitTests/OPStest/UnitTests/test-subscribeUDP  	--gtest_output="xml:$UNIT_DIR/Unittest-publish-and-subscribe_result_udp.xml" &

sleep 1
build.debug/UnitTests/OPStest/src/publishBin

sleep 10
mkdir -p Unit_test_results/Coverage
gcovr --root . -b --xml --xml-pretty -o $JENK_DIR/coverage_result_OPS_jenkins.xml


bash
	cppcheck --enable=all --inconclusive --xml --xml-version=2 Cpp/source Cpp/include 2> $JENK_DIR/cppcheck_result_OPS_jenkins.xml
exit


