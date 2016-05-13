
# list of source files that, when changed, require regeneration of target
set(HELLO_SRCS
    opsproject/project.properties
    src/hello/HelloData.idl
    )

# the output file that we want
set(JAR_OUTPUT ${CMAKE_BINARY_DIR}/bin/HelloWorld.jar)

add_custom_command(OUTPUT ${JAR_OUTPUT}
  COMMAND ${OPS_COMPILER} -P ${CMAKE_CURRENT_SOURCE_DIR} -o ${CMAKE_CURRENT_BINARY_DIR}
  COMMAND ${CMAKE_COMMAND} -E copy_if_different Java/HelloWorld.jar ${JAR_OUTPUT}
  DEPENDS ${HELLO_SRCS}
  COMMENT "Generating ${JAR_OUTPUT} from ${CMAKE_CURRENT_SOURCE_DIR}"
  VERBATIM
  )

add_custom_target(HelloWorld DEPENDS ${JAR_OUTPUT})
