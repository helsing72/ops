
set(APP_SRC
  simplechatsenderjavaimpl/Main.java
)

# generate a MANIFEST file
configure_file(MANIFEST.MF.in MANIFEST.MF)

add_jar(ChatSenderJavaImpl ${APP_SRC}
  INCLUDE_JARS ${PROJECT_BINARY_DIR}/bin/ChatExample.jar OPSJLib ConfigurationLib
  ENTRY_POINT simplechatsenderjavaimpl/Main
  MANIFEST ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)

add_dependencies(ChatSenderJavaImpl ChatExample)

install(FILES
  ${CMAKE_BINARY_DIR}/bin/ChatSenderJavaImpl-${PROJECT_VERSION}.jar
  ${CMAKE_BINARY_DIR}/bin/OPSJLib-${PROJECT_VERSION}.jar
  ${CMAKE_BINARY_DIR}/bin/ConfigurationLib-${PROJECT_VERSION}.jar
  ${PROJECT_BINARY_DIR}/bin/ChatExample.jar
  ${OPSIDL_SOURCE_DIR}/ChatExample/src/ops_config.xml
  DESTINATION examples/chatexample
)
