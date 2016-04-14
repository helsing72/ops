cmake_minimum_required(VERSION 3.0 FATAL_ERROR)

set(LIB_NAME UnitTests)
set(_ECLIPSE_VERSION "4.4.2")
set(CMAKE_MODULE_PATH ${CMAKE_MODULE_PATH} ${CMAKE_SOURCE_DIR})

if(NOT DEFINED ENV{GOOGLETEST_HOME})
	message(FATAL_ERROR "GOOGLETEST_HOME is undefined")
endif()

if(NOT DEFINED ENV{GCOVR_HOME})
	message(FATAL_ERROR "GCOVR_HOME is undefined")
endif()

if(NOT DEFINED ENV{BOOST_HOME})
	message(FATAL_ERROR "BOOST_HOME is undefined")
endif()

if(NOT DEFINED ENV{CPPCHECK})
	message(FATAL_ERROR "CPPCHECK is undefined")
endif()

if(CMAKE_BUILD_TYPE MATCHES "Debug")
	add_custom_target(UnitTests DEPENDS 
	publishBin
	test-subscribeTCP
	test-subscribeUDP
	test-subscribe
	test-serialize-and-deserialize
	)
endif()

#set OPS variables
set(OPS_INCLUDE_DIR ${CMAKE_CURRENT_SOURCE_DIR}/../../Cpp/include)
set(OPS_LIBRARY 	${CMAKE_CURRENT_SOURCE_DIR}/../../build.debug/Cpp/source/libopsdbg.so)

#set google-test variables
set(GTEST_LIBRARIES $ENV{GOOGLETEST_LIB}/libgtest.a)
set(GTEST_INCLUDE_DIR $ENV{GOOGLETEST_INCLUDE})


message("GTEST_INCLUDE_DIR is ${GTEST_INCLUDE_DIR}")
message("GTEST_LIBRARIES is ${GTEST_LIBRARIES}")


project (OPStest)


#find_package(OPS)

include_directories(lib)
add_subdirectory(src)
add_subdirectory(lib)
add_subdirectory(UnitTests)

