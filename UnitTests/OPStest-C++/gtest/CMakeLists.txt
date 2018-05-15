
set(APP_NAME gtest-main)

set(APP_SRCS
  ByteBuffer_test.cpp
  ErrorServiceWriter_test.cpp
  Filter_test.cpp
  fixed_string_test.cpp
  gtest_main.cpp
  MemoryMap_test.cpp
  NotifierListener_test.cpp
  ObjectFactory_test.cpp
  OpsObject_test.cpp
  OPSUtilities_test.cpp
  Reservable_test.cpp
  Serialization_test.cpp
)

# We need also the internal OPS C++ headers from:
include_directories(${CMAKE_SOURCE_DIR}/Cpp/source)

add_executable(${APP_NAME} ${APP_SRCS})

target_link_libraries(${APP_NAME} ${Boost_LIBRARIES} ${GTEST_LIBRARIES} ${CMAKE_THREAD_LIBS_INIT} ops-static)

if(MSVC)
  target_link_libraries(${APP_NAME} Winmm)
elseif(MINGW)
  target_link_libraries(${APP_NAME} ws2_32 mswsock)
else()
  target_link_libraries(${APP_NAME} pthread rt)
endif()

install(TARGETS ${APP_NAME} DESTINATION test)
