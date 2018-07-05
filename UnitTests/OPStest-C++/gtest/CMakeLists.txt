
set(APP_NAME gtest-main)
set(APP2_NAME gtest-mempool)

set(APP_SRCS
  ByteBuffer_test.cpp
  ErrorServiceWriter_test.cpp
  Filter_test.cpp
  fixed_string_test.cpp
  gtest_main.cpp
  memory_pool_test.cpp
  MemoryMap_test.cpp
  NotifierListener_test.cpp
  ObjectFactory_test.cpp
  OpsConfigObjects_test.cpp
  OpsConfigRepo_test.cpp
  OpsMessage_test.cpp
  OpsObject_test.cpp
  OPSUtilities_test.cpp
  ParticipantInfo_test.cpp
  PubIdChecker_test.cpp
  Reservable_test.cpp
  Serialization_test.cpp
  SetupOPSConfig.cpp
  SetupOPSConfig.h
)

set(APP2_SRCS
  gtest_main.cpp
  memory_pool_nd_test.cpp
)

# We need also the internal OPS C++ headers from:
include_directories(
  ${CMAKE_SOURCE_DIR}/Cpp/source
  ${CMAKE_SOURCE_DIR}/Common/idl/Generated/Cpp
)

add_executable(${APP_NAME} ${APP_SRCS})
add_executable(${APP2_NAME} ${APP2_SRCS})

target_link_libraries(${APP_NAME} ${Boost_LIBRARIES} ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ops-static)
target_link_libraries(${APP2_NAME} ${Boost_LIBRARIES} ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ops-static)

if(MSVC)
  target_link_libraries(${APP_NAME} Winmm)
  target_link_libraries(${APP2_NAME} Winmm)
elseif(MINGW)
  target_link_libraries(${APP_NAME} ws2_32 mswsock)
  target_link_libraries(${APP2_NAME} ws2_32 mswsock)
else()
  target_link_libraries(${APP_NAME} pthread rt)
  target_link_libraries(${APP2_NAME} pthread rt)
endif()

install(TARGETS ${APP_NAME} DESTINATION test)
install(TARGETS ${APP2_NAME} DESTINATION test)

