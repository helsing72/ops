
# list of source files that, when changed, require regeneration of target
set(CURRENT_SRCS
    opsproject/project.properties
    src/hello/HelloData.idl
    src/hello/RequestHelloData.idl
    )

# the output file that we want
set(JAR_OUTPUT ${CMAKE_BINARY_DIR}/bin/HelloRequestReply.jar)
# use opsc script from build tree
if(WIN32)
  set(OPS_COMPILER ${CMAKE_BINARY_DIR}/bin/opsc.bat)
else()
  set(OPS_COMPILER ${CMAKE_BINARY_DIR}/bin/opsc.sh)
endif()

add_custom_command(OUTPUT ${JAR_OUTPUT}
  COMMAND ${OPS_COMPILER} -P ${CMAKE_CURRENT_SOURCE_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/Generated/Java/HelloRequestReply.jar ${JAR_OUTPUT}
  DEPENDS ${CURRENT_SRCS}
  COMMENT "Generating ${JAR_OUTPUT} from ${CMAKE_CURRENT_SOURCE_DIR}"
  VERBATIM
  )

add_custom_target(HelloRequestReply DEPENDS ${JAR_OUTPUT})
