
set(APP_SRC
  simplechatsenderjavaimpl/Main.java
)

# generate a MANIFEST file
configure_file(MANIFEST.MF.in MANIFEST.MF)

add_jar(ChatSenderJavaImpl ${APP_SRC}
  INCLUDE_JARS ${PROJECT_BINARY_DIR}/bin/ChatExample.jar ${OPSJLIB_JAR} ${CONFIGURATIONLIB_JAR}
  ENTRY_POINT simplechatsenderjavaimpl/Main
  MANIFEST ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)

add_dependencies(ChatSenderJavaImpl ChatExample)

install(FILES
  ${CMAKE_BINARY_DIR}/bin/ChatSenderJavaImpl-${PROJECT_VERSION}.jar
  ${OPSJAR_PATH}/OPSJLib-${PROJECT_VERSION}.jar
  ${OPSJAR_PATH}/ConfigurationLib-${PROJECT_VERSION}.jar
  ${PROJECT_BINARY_DIR}/bin/ChatExample.jar
  DESTINATION examples/chatexample
)
