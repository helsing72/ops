
set(APP_NAME Example1Cpp)

include_directories(${OPSIDL_SOURCE_DIR}/TestAll/Generated/Cpp)

set(APP_SRCS
  Example1.cpp
)

add_executable(${APP_NAME} ${APP_SRCS})

# generate TestAll ops idl project if necessary
add_dependencies(${APP_NAME} TestAll)

set_target_properties(${APP_NAME} PROPERTIES VERSION ${PROJECT_VERSION})
#set_target_properties(${LIB_NAME} PROPERTIES DEBUG_POSTFIX "-dbg")
#set_target_properties(${LIB_NAME} PROPERTIES RELEASE_POSTFIX "-opt")

target_link_libraries(${APP_NAME} ${Boost_LIBRARIES} ops-static)

# under visual studio the time functions uses function in Winmm.lib
if(MSVC)
  target_link_libraries(${APP_NAME} Winmm)
elseif(MINGW)
  target_link_libraries(${APP_NAME} ws2_32 mswsock)
else()
  target_link_libraries(${APP_NAME} pthread rt)
endif()

install(TARGETS ${APP_NAME} DESTINATION examples/testall)

file(INSTALL ${OPSIDL_SOURCE_DIR}/TestAll/src/ops_config.xml DESTINATION ${CMAKE_INSTALL_PREFIX}/examples/testall)
