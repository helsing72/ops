
set(APP_NAME PizzaTestCpp)

include_directories(${OPSIDL_SOURCE_DIR}/PizzaProject/Generated/Cpp)

set(APP_SRCS
  PizzaTest.cpp
  SdsSystemTime.cpp
  SdsSystemTime.h
  targetver.h
)

add_executable(${APP_NAME} ${APP_SRCS})

# generate PizzaProject ops idl project if necessary
add_dependencies(${APP_NAME} PizzaProject)

set_target_properties(${APP_NAME} PROPERTIES VERSION ${PROJECT_VERSION})
#set_target_properties(${LIB_NAME} PROPERTIES DEBUG_POSTFIX "-dbg")
#set_target_properties(${LIB_NAME} PROPERTIES RELEASE_POSTFIX "-opt")

target_link_libraries(${APP_NAME} ${Boost_LIBRARIES} ops-static)

# under visual studio the time functions uses function in Winmm.lib
if(MSVC)
  target_link_libraries(${APP_NAME} Winmm)
endif()

if(MINGW)
  target_link_libraries(${APP_NAME} ws2_32 mswsock)
endif()

install(TARGETS ${APP_NAME} DESTINATION examples/pizzatest)

file(INSTALL Debug/ops_config.xml DESTINATION ${CMAKE_INSTALL_PREFIX}/examples/pizzatest)
