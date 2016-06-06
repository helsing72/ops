
set(APP_SRC
  helloreplierjavaimpl/Main.java
)

# generate a MANIFEST file
configure_file(MANIFEST.MF.in MANIFEST.MF)

add_jar(HelloReplierJavaImpl ${APP_SRC}
  INCLUDE_JARS ${PROJECT_BINARY_DIR}/bin/HelloRequestReply.jar OPSJLib ConfigurationLib
  ENTRY_POINT helloreplierjavaimpl/Main
  MANIFEST ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)

add_dependencies(HelloReplierJavaImpl HelloRequestReply)

install(FILES
  ${CMAKE_BINARY_DIR}/bin/HelloReplierJavaImpl-${PROJECT_VERSION}.jar
  ${CMAKE_BINARY_DIR}/bin/OPSJLib-${PROJECT_VERSION}.jar
  ${CMAKE_BINARY_DIR}/bin/ConfigurationLib-${PROJECT_VERSION}.jar
  ${PROJECT_BINARY_DIR}/bin/HelloRequestReply.jar
  ${OPSIDL_SOURCE_DIR}/HelloRequestReply/src/ops_config.xml
  DESTINATION examples/requesthello
)
