
# list of source files that, when changed, require regeneration of target
set(CURRENT_SRCS
    opsproject/project.properties
    src/chat/ChatData.idl
    src/chat/ExtendedChatData.idl
    src/chat/ExtendedUserData.idl
    src/chat/MessageData.idl
    src/chat/UserData.idl
    )

# the output file that we want
set(JAR_OUTPUT ${CMAKE_BINARY_DIR}/bin/ChatExample.jar)

add_custom_command(OUTPUT ${JAR_OUTPUT}
  COMMAND ${OPS_COMPILER} -P ${CMAKE_CURRENT_SOURCE_DIR} -gpr ../../../../../Ada/
  COMMAND ${CMAKE_COMMAND} -E copy_if_different ${CMAKE_CURRENT_SOURCE_DIR}/Generated/Java/ChatExample.jar ${JAR_OUTPUT}
  DEPENDS ${CURRENT_SRCS}
  COMMENT "Generating ${JAR_OUTPUT} from ${CMAKE_CURRENT_SOURCE_DIR}"
  VERBATIM
  )

add_custom_target(ChatExample ALL DEPENDS ${JAR_OUTPUT})
add_dependencies(ChatExample opsc)
