
#(UnitTests - CMakeLists.txt)

add_executable(test-subscribe SubscribeDataAndTest.cpp)
add_executable(test-subscribeTCP SubscribeTCPDataAndTest.cpp)
add_executable(test-subscribeUDP SubscribeUDPDataAndTest.cpp)
add_executable(test-serialize-and-deserialize TestSerializeAndDeserialize.cpp)

target_link_libraries(test-subscribe                 ${Boost_LIBRARIES} ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ops)
target_link_libraries(test-subscribeTCP              ${Boost_LIBRARIES} ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ops)
target_link_libraries(test-subscribeUDP              ${Boost_LIBRARIES} ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ops)
target_link_libraries(test-serialize-and-deserialize ${Boost_LIBRARIES} ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ops)