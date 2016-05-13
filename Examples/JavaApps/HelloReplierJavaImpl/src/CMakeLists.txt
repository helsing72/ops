
set(APP_SRC
  helloreplierjavaimpl/Main.java
)

#INCLUDE_JARS OPSCompilerLib OPSJLib IDLParser ConfigurationLib IDLTemplates

add_jar(HelloReplierJavaImpl ${APP_SRC}
  INCLUDE_JARS ${PROJECT_BINARY_DIR}/bin/HelloRequestReply.jar OPSJLib ConfigurationLib
  ENTRY_POINT helloreplierjavaimpl/Main
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)

add_dependencies(HelloReplierJavaImpl HelloRequestReply)

# in cmake-3.1 (at least) the install_jar is broken for Windows
#if(UNIX)
#  install_jar(opsc ${OPS_JAR_REL_INSTALL_PATH})
#else()
  # install manually
#  set(JAR_OUTPUT ${CMAKE_BINARY_DIR}/bin/opsc-${OPS_VERSION}.jar)
#  install(FILES ${JAR_OUTPUT} DESTINATION ${OPS_JAR_REL_INSTALL_PATH})
#  install(FILES ${JAR_OUTPUT} DESTINATION ${OPS_JAR_REL_INSTALL_PATH} RENAME opsc.jar)
#endif()
