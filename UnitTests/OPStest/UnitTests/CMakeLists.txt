
#(UnitTests - CMakeLists.txt)

include_directories(${Boost_INCLUDE_DIRS})
include_directories(${GTEST_INCLUDE_DIR})
include_directories(${OPS_INCLUDE_DIR})
include_directories(${OPENTHREADS_INCLUDE_DIRS} )

add_executable(test-subscribe SubscribeDataAndTest.cpp)
add_executable(test-subscribeTCP SubscribeTCPDataAndTest.cpp)
add_executable(test-subscribeUDP SubscribeUDPDataAndTest.cpp)
add_executable(test-serialize-and-deserialize TestSerializeAndDeserialize.cpp)
add_dependencies(test-subscribe opsdbg)
add_dependencies(test-subscribeTCP opsdbg)
add_dependencies(test-subscribeUDP opsdbg)
add_dependencies(test-serialize-and-deserialize opsdbg)

target_link_libraries(test-subscribe                 ${Boost_LIBRARIES} boost_system boost_thread pthread --coverage ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${OPS_LIBRARY})
target_link_libraries(test-subscribeTCP              ${Boost_LIBRARIES} boost_system boost_thread pthread --coverage ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${OPS_LIBRARY})
target_link_libraries(test-subscribeUDP              ${Boost_LIBRARIES} boost_system boost_thread pthread --coverage ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${OPS_LIBRARY})
target_link_libraries(test-serialize-and-deserialize ${Boost_LIBRARIES} boost_system boost_thread pthread --coverage ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ${OPS_LIBRARY})
