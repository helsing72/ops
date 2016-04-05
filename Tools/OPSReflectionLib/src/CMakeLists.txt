
set(OPSREFLECTIONLIB_SRC
  opsreflection/JarPathConfig.java
  opsreflection/OPSFactoryConfig.java
  opsreflection/OPSFactory.java
  opsreflection/OPSReflectionSerializableFactory.java
)

add_jar(OPSReflectionLib ${OPSREFLECTIONLIB_SRC}
  INCLUDE_JARS ConfigurationLib JarSearch OPSCompilerLib OPSJLib
  VERSION ${OPS_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)


install_jar(OPSReflectionLib ${OPS_JAR_REL_INSTALL_PATH})

if(BUILD_DOCUMENTATION)
  create_javadoc(OPSReflectionLib
    PACKAGES opsreflection
    SOURCEPATH "${CMAKE_CURRENT_SOURCE_DIR}"
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH}
    WINDOWTITLE "OPSReflectionLib documentation"
    DOCTITLE "<h1>OPSReflectionLib documentation</h1>"
    AUTHOR TRUE
    USE TRUE
    VERSION TRUE
    )
endif()
