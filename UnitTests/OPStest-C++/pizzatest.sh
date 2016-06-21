#!/bin/bash

cd ../../

exit_count=0
exit_des_ser=0
exit_normal=0
exit_tcp=0
exit_udp=0
#remove gcda files build folder
find build.debug/ -name '*.gcda' | xargs rm -f

mkdir -p UnitTests/OPStest-C++/Unit_test_results
date > UnitTests/OPStest-C++/Unit_test_results/UnitTests-result.txt 
build.debug/UnitTests/OPStest-C++/UnitTests/test-serialize-and-deserialize >> UnitTests/OPStest-C++/Unit_test_results/UnitTests-result.txt
exit_des_ser=$?

build.debug/UnitTests/OPStest-C++/UnitTests/test-subscribe    & #>> UnitTests/OPStest-C++/Unit_test_results/UnitTests-result.txt &
pid_normal=$!

build.debug/UnitTests/OPStest-C++/UnitTests/test-subscribeTCP & # >> UnitTests/OPStest-C++/Unit_test_results/UnitTests-result.txt &
pid_tcp=$!

build.debug/UnitTests/OPStest-C++/UnitTests/test-subscribeUDP & #>> UnitTests/OPStest-C++/Unit_test_results/UnitTests-result.txt &
pid_udp=$!

sleep 1
build.debug/UnitTests/OPStest-C++/src/publishBin              >> UnitTests/OPStest-C++/Unit_test_results/UnitTests-result.txt

wait $pid_normal
exit_normal=$?

wait $pid_tcp
exit_tcp=$?

wait $pid_udp
exit_udp=$?

mkdir -p UnitTests/OPStest-C++/Unit_test_results/Coverage
gcovr -r . --html --html-details -o UnitTests/OPStest-C++/Unit_test_results/Coverage/coverage_result_OPS.html

cppcheck --enable=all Cpp/source Cpp/include 2> UnitTests/OPStest-C++/Unit_test_results/SA_ERRORS.txt > /dev/null
exit_count=$(( $exit_des_ser + $exit_normal + $exit_tcp  + $exit_udp ))

if [ $exit_count == 0 ]
then
	printf "\n\n   ALL TEST PASSED\n\n\n"
else
	printf "\n\n   $exit_count TEST FAILED\n\n\n"
fi
