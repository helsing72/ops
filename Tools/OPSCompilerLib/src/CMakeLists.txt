
set(OPSCOMPILERLIB_SRC
  parsing/AbstractTemplateBasedIDLCompiler.java
  parsing/IDLClass.java
  parsing/IDLCompiler.java
  parsing/IDLField.java
  parsing/TopicInfo.java
  parsing/Topic.java
  gui/IDLCompilerSettingsPlugin.java
)

add_jar(OPSCompilerLib ${OPSCOMPILERLIB_SRC}
  VERSION ${OPS_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)

install_jar(OPSCompilerLib ${OPS_JAR_REL_INSTALL_PATH})

if(BUILD_DOCUMENTATION)
  create_javadoc(OPSCompilerLib
    PACKAGES gui parsing
    SOURCEPATH "${CMAKE_CURRENT_SOURCE_DIR}"
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH}
    WINDOWTITLE "OPSCompilerLib documentation"
    DOCTITLE "<h1>OPSCompilerLib documentation</h1>"
    AUTHOR TRUE
    USE TRUE
    VERSION TRUE
    )
endif()
