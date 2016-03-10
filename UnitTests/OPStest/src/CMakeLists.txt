
#(src - CMakeLists.txt)

set (CMAKE_EXE_LINKER_FLAGS "-Wl --as-needed")
#find_package(Boost COMPONENTS thread system filesystem REQUIRED)


#set (Boost_USE_STATIC_LIBS ON)
set (Boost_USE_MULTITHREADED ON)
set (Boost_USE_STATIC_RUNTIME OFF)



cmake_minimum_required(VERSION 3.0 FATAL_ERROR)
set(_ECLIPSE_VERSION "4.4.2")
project (OPStest)




set(HEADER_PATH ${CMAKE_CURRENT_SOURCE_DIR})

include_directories(${Boost_INCLUDE_DIRS} ${OPS_INCLUDE_DIR} ${OPENTHREADS_INCLUDE_DIRS})

add_executable(publishBin PublishData.cpp)

target_link_libraries(publishBin ${Boost_LIBRARIES} boost_system boost_thread pthread ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${OPS_LIBRARY})
