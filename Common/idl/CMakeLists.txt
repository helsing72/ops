
set(BUILD_NAME opsidls)

# list of source files that, when changed, require regeneration of target
set(IDL_SRCS
    ${CMAKE_CURRENT_SOURCE_DIR}/DebugRequestResponseData.idl
    )

# the output file that we want
set(JAR_OUTPUT ${CMAKE_BINARY_DIR}/bin/opsidls.jar)

add_custom_command(OUTPUT ${JAR_OUTPUT}
  COMMAND ${OPS_COMPILER} -g ALL -b java -o ${CMAKE_CURRENT_SOURCE_DIR}/Generated -p opsidls ${IDL_SRCS}
  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/Generated/Java/opsidls.jar ${JAR_OUTPUT}
  DEPENDS ${IDL_SRCS}
  COMMENT "Generating ${JAR_OUTPUT} from ${CMAKE_CURRENT_SOURCE_DIR}"
  VERBATIM
  )

add_custom_target(${BUILD_NAME} DEPENDS ${JAR_OUTPUT})
add_dependencies(${BUILD_NAME} opsc)
