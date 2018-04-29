#!/bin/bash
cd ../../

exit_count=0
exit_gtest=0
exit_des_ser=0
exit_normal=0
exit_tcp=0
exit_udp=0
#remove gcda files build folder
find build.debug/ -name '*.gcda' | xargs rm -f

mkdir -p UnitTests/OPStest-C++/Unit_test_results
date > UnitTests/OPStest-C++/Unit_test_results/UnitTests-result.txt
build.debug/UnitTests/OPStest-C++/gtest/gtest-main --gtest_output="xml:unittest-c++-gtest-result.xml"
exit_gtest=$?

build.debug/UnitTests/OPStest-C++/UnitTests/test-serialize-and-deserialize --gtest_output="xml:unittest-c++-ser-deser-result.xml"
exit_des_ser=$?

build.debug/UnitTests/OPStest-C++/UnitTests/test-subscribe    --gtest_output="xml:unittest-c++-pub-sub-result.xml"     &
pid_normal=$!

build.debug/UnitTests/OPStest-C++/UnitTests/test-subscribeTCP --gtest_output="xml:unittest-c++-pub-sub-result-tcp.xml" &
pid_tcp=$!

build.debug/UnitTests/OPStest-C++/UnitTests/test-subscribeUDP --gtest_output="xml:unittest-c++-pub-sub-result-udp.xml" &
pid_udp=$!

sleep 1
build.debug/UnitTests/OPStest-C++/src/publishBin

wait $pid_normal
exit_normal=$?

wait $pid_tcp
exit_tcp=$?

wait $pid_udp
exit_udp=$?

sleep 3
gcovr --root . -b --xml --xml-pretty -o coverage-c++-result.xml

bash
	cppcheck --enable=all --inconclusive --xml --xml-version=2 Cpp/source Cpp/include 2> cppcheck-c++-result.xml
exit

exit_count=$(( $exit_gtest + $exit_des_ser + $exit_normal + $exit_tcp  + $exit_udp ))

if [ $exit_count == 0 ]
then
	printf "\n\n   ALL TEST PASSED\n\n\n"
else
	printf "\n\n   $exit_count TEST FAILED\n\n\n"
fi
