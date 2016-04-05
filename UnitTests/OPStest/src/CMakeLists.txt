#(src - CMakeLists.txt)

cmake_minimum_required(VERSION 3.0 FATAL_ERROR)
set(_ECLIPSE_VERSION "4.4.2")
project (OPStest)

include_directories(${Boost_INCLUDE_DIRS} ${OPS_INCLUDE_DIR} ${OPENTHREADS_INCLUDE_DIRS})

add_executable(publishBin PublishData.cpp)
add_dependencies(publishBin opsdbg)

target_link_libraries(publishBin ${Boost_LIBRARIES} boost_system boost_thread pthread --coverage ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${OPS_LIBRARY})
