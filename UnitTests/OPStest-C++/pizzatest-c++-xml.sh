#!/bin/bash

cd ../..
JENK_DIR="/tmp/jenkins/workspace/ops_pizzatest"

build.debug/UnitTests/OPStest/UnitTests/test-serialize-and-deserialize --gtest_output="xml:$JENK_DIR/unittest-c++-ser-deser-result.xml"

build.debug/UnitTests/OPStest/UnitTests/test-subscribe  	--gtest_output="xml:$JENK_DIR/unittest-c++-pub-sub-result.xml"     &
build.debug/UnitTests/OPStest/UnitTests/test-subscribeTCP	--gtest_output="xml:$JENK_DIR/unittest-c++-pub-sub-result-tcp.xml" &
build.debug/UnitTests/OPStest/UnitTests/test-subscribeUDP  	--gtest_output="xml:$JENK_DIR/unittest-c++-pub-sub-result-udp.xml" &

sleep 1
build.debug/UnitTests/OPStest/src/publishBin

sleep 3
gcovr --root . -b --xml --xml-pretty -o $JENK_DIR/coverage-c++-result.xml

bash
	cppcheck --enable=all --inconclusive --xml --xml-version=2 Cpp/source Cpp/include 2> $JENK_DIR/cppcheck-c++-result.xml
exit

