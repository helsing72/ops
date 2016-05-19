
set(PUB_NAME HelloWorldPub)
set(SUB_NAME HelloWorldSub)

include_directories(${CMAKE_SOURCE_DIR}/Examples/OPSIdls/HelloWorld/Generated/Cpp)

# 
add_executable(${PUB_NAME} HelloTopic_pub.cpp)
add_executable(${SUB_NAME} HelloTopic_sub.cpp)

# generate HelloWorld ops idl project if necessary
add_dependencies(${PUB_NAME} HelloWorld)
add_dependencies(${SUB_NAME} HelloWorld)

set_target_properties(${PUB_NAME} PROPERTIES VERSION ${PROJECT_VERSION})
set_target_properties(${SUB_NAME} PROPERTIES VERSION ${PROJECT_VERSION})

target_link_libraries(${PUB_NAME} ${Boost_LIBRARIES} ops-static)
target_link_libraries(${SUB_NAME} ${Boost_LIBRARIES} ops-static)

if(MINGW)
  target_link_libraries(${PUB_NAME} ws2_32 mswsock)
endif()

install(TARGETS ${PUB_NAME} ${SUB_NAME} DESTINATION bin)
