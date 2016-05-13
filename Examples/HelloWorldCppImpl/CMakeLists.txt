
set(PUB_NAME HelloWorldPub)
set(SUB_NAME HelloWorldSub)

include_directories(${CMAKE_BINARY_DIR}/Examples/OPSIdls/HelloWorld/Cpp)

# 
add_executable(${PUB_NAME} HelloTopic_pub.cpp)
add_executable(${SUB_NAME} HelloTopic_sub.cpp)

# generate HelloWorld ops idl project if necessary
add_dependencies(${PUB_NAME} HelloWorld)
add_dependencies(${SUB_NAME} HelloWorld)

set_target_properties(${PUB_NAME} PROPERTIES VERSION ${PROJECT_VERSION})
#set_target_properties(${LIB_NAME} PROPERTIES DEBUG_POSTFIX "-dbg")
#set_target_properties(${LIB_NAME} PROPERTIES RELEASE_POSTFIX "-opt")

target_link_libraries(${PUB_NAME} ${Boost_LIBRARIES} ops)

if(MINGW)
  target_link_libraries(${PUB_NAME} ws2_32 mswsock)
endif()

install(TARGETS ${PUB_NAME} ${SUB_NAME} DESTINATION bin)
