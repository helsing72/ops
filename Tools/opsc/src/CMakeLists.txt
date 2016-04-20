
set(OPSC_SRC
  # opsc
  opsc/OpsCompiler.java
  opsc/Compiler.java
  opsc/CSharpCompiler.java
  opsc/CppCompiler.java
  opsc/JavaCompiler.java
  opsc/IDLParser.java
  opsc/ProjectProperties.java
  opsc/Property.java
  opsc/JarDependency.java
)

# generate a MANIFEST file
configure_file(MANIFEST.MF.in MANIFEST.MF)

add_jar(opsc ${OPSC_SRC}
  INCLUDE_JARS OPSCompilerLib OPSJLib IDLParser ConfigurationLib IDLTemplates
  ENTRY_POINT opsc/OpsCompiler
  MANIFEST ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)

# in cmake-3.1 (at least) the install_jar is broken for Windows
if(UNIX)
  install_jar(opsc ${OPS_JAR_REL_INSTALL_PATH})
else()
  # install manually
  set(JAR_OUTPUT ${CMAKE_BINARY_DIR}/bin/opsc-${OPS_VERSION}.jar)
  install(FILES ${JAR_OUTPUT} DESTINATION ${OPS_JAR_REL_INSTALL_PATH})
  install(FILES ${JAR_OUTPUT} DESTINATION ${OPS_JAR_REL_INSTALL_PATH} RENAME opsc.jar)
endif()

if(BUILD_DOCUMENTATION)
  create_javadoc(opsc
    PACKAGES opsc
    SOURCEPATH "${CMAKE_CURRENT_SOURCE_DIR}"
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH}
    WINDOWTITLE "ops idl compiler documentation"
    DOCTITLE "<h1>ops idl compiler documentation</h1>"
    AUTHOR TRUE
    USE TRUE
    VERSION TRUE
    )
endif()
