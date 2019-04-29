
# list of source files that, when changed, require regeneration of target
set(CURRENT_SRCS
    opsproject/project.properties
    src/BaseIDLs/BaseData.idl
    src/BaseIDLs/CommonData.idl
    )

# the output file that we want
set(JAR_OUTPUT ${CMAKE_BINARY_DIR}/bin/BaseIDLs.jar)

add_custom_command(OUTPUT ${JAR_OUTPUT}
  COMMAND ${OPS_COMPILER} -P ${CMAKE_CURRENT_SOURCE_DIR} -gpr ../../../../../../Ada/
  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/Generated/Java/BaseIDLs.jar ${JAR_OUTPUT}
  DEPENDS ${CURRENT_SRCS}
  COMMENT "Generating ${JAR_OUTPUT} from ${CMAKE_CURRENT_SOURCE_DIR}"
  VERBATIM
  )

add_custom_target(BaseIDLs ALL DEPENDS ${JAR_OUTPUT})
add_dependencies(BaseIDLs opsc)
