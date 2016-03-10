set(_ECLIPSE_VERSION "4.4.2")
cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR})

if(NOT DEFINED ENV{GOOGLETEST_HOME})
	message(FATAL_ERROR "google-test module must be added, do it by typing module add google-test/1.7")
endif()

if(NOT DEFINED ENV{GCOVR_HOME})
	message(FATAL_ERROR "gcovr module must be added, do it by typing module add gcovr/3.2")
endif()

if(NOT DEFINED ENV{BOOST_HOME})
	message(FATAL_ERROR "boost module must be added, do it by typing module add boost/1.55")
endif()



#set OPS variables
set(OPS_INCLUDE_DIR ../../Cpp/include)
set(OPS_LIBRARY 	../../Cpp/lib64/libOPSopt.a)

#set google-test variables
set(GTEST_LIBRARIES $ENV{GOOGLETEST_HOME}/google-build/googlemock/gtest/libgtest.a)
set(GTEST_INCLUDE_DIR ${TOOLS}/$ENV{GOOGLETEST_HOME}/googletest/include)

#set boost variables
set(Boost_INCLUDE_DIRS $ENV{BOOST_HOME}/boost)
set(Boost_LIBRARIES $ENV{BOOST_HOME}/lib64)
link_directories($ENV{BOOST_HOME}/lib64)
#set(BOOST_ROOT $ENV{BOOST_HOME}) #need this?


#set compiler flags
set(CMAKE_CXX_FLAGS "-fprofile-arcs -ftest-coverage -fPIC -O0")


project (OPStest)


find_package(OPS)

include_directories(lib)
add_subdirectory(src)
add_subdirectory(lib)
add_subdirectory(UnitTests)
