
# use opsc script from build tree, will be used in all subdirs under this one
if(WIN32)
  set(OPS_COMPILER ${CMAKE_BINARY_DIR}/bin/opsc.bat)
else()
  set(OPS_COMPILER ${CMAKE_BINARY_DIR}/bin/opsc.sh)
endif()

# this one generates hello.jar
add_subdirectory(HelloRequestReply)

add_subdirectory(HelloWorld)

add_subdirectory(PizzaProject)
