#!/bin/bash -l

cd OPStest-Python/

echo "RUNNING PYTHON TESTS"
./pizzatest-xml.sh
sleep 20
cd ../OPStest-C++
echo "RUNNING C++ TESTS"
./pizzatest-xml.sh
