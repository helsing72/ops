#!/bin/bash

cd ../../

exit_count=0
exit_des_ser=0
exit_normal=0
exit_tcp=0
exit_udp=0
#remove gcda files build folder
find build.debug/ -name '*.gcda' | xargs rm -f

date > Unit_test_results/UnitTests-result.txt 
build.debug/UnitTests/OPStest/UnitTests/test-serialize-and-deserialize >> Unit_test_results/UnitTests-result.txt
exit_des_ser=$?

build.debug/UnitTests/OPStest/UnitTests/test-subscribe    >> Unit_test_results/UnitTests-result.txt &
pid_normal=$!

build.debug/UnitTests/OPStest/UnitTests/test-subscribeTCP >> Unit_test_results/UnitTests-result.txt &
pid_tcp=$!

build.debug/UnitTests/OPStest/UnitTests/test-subscribeUDP >> Unit_test_results/UnitTests-result.txt &
pid_udp=$!

sleep 1
build.debug/UnitTests/OPStest/src/publishBin              >> Unit_test_results/UnitTests-result.txt

wait $pid_normal
exit_normal=$?

wait $pid_tcp
exit_tcp=$?

wait $pid_udp
exit_udp=$?

mkdir -p Unit_test_results/Coverage
gcovr -r . --html --html-details -o Unit_test_results/Coverage/coverage_result_OPS.html

cppcheck --enable=all Cpp/source Cpp/include 2> Unit_test_results/SA_ERRORS.txt > /dev/null
exit_count=$(( $exit_des_ser + $exit_normal + $exit_tcp  + $exit_udp ))

if [ $exit_count == 0 ]
then
	printf "\n\n   ALL TEST PASSED\n\n\n"
else
	printf "\n\n   $exit_count TEST FAILED\n\n\n"
fi
