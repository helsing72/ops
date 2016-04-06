#!/bin/bash

cd ../../

#remove gcda files build folder
find build.debug/ -name '*.gcda' | xargs rm -f

date > Unit_test_results/UnitTests-result.txt
build.debug/UnitTests/OPStest/UnitTests/test-serialize-and-deserialize >> Unit_test_results/UnitTests-result.txt

build.debug/UnitTests/OPStest/UnitTests/test-subscribe    >> Unit_test_results/UnitTests-result.txt &
build.debug/UnitTests/OPStest/UnitTests/test-subscribeTCP >> Unit_test_results/UnitTests-result.txt &
build.debug/UnitTests/OPStest/UnitTests/test-subscribeUDP >> Unit_test_results/UnitTests-result.txt &
sleep 1
build.debug/UnitTests/OPStest/src/publishBin              >> Unit_test_results/UnitTests-result.txt

mkdir -p Unit_test_results/Coverage
gcovr -r . --html --html-details -o Unit_test_results/Coverage/coverage_result_OPS.html

cppcheck --enable=all Cpp/source Cpp/include 2> Unit_test_results/SA_ERRORS.txt > /dev/null
echo "Done!"
