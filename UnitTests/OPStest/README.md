In order to build and run OPS unit tests, make sure that you have CMake 3.1, module boost, google-test and gcovr loaded

In order to get coverage you have to compile OPS with the following flags, -fprofile-arcs -ftest-coverage -O0 -fPIC, go to ops4/Cpp and open makefile_linux.include

add -fprofile-arcs -ftest-coverage -O0 -fPIC to the compile command

run
make clean -f makefile_linux_x64
make -f makefile_linux_x64

To compile the Unit test code, go to ops4/UnitTests/OPStest/  and run the commands
mkdir build
cd build
cmake ..
make

(If you already have a build remove all contents in it)

This will generate binaries in the build directory
To run the tests, run

cd ..
pizzatest.sh . 

(note the '.')


