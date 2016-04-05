
set(JARSEARCH_SRC
  jarsearch/JarSearcher.java
  jarsearch/util/FileHelper.java
)

add_jar(JarSearch ${JARSEARCH_SRC}
  VERSION ${OPS_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)

install_jar(JarSearch ${OPS_JAR_REL_INSTALL_PATH})

if(BUILD_DOCUMENTATION)
  create_javadoc(jarsearch
    PACKAGES jarsearch
    SOURCEPATH "${CMAKE_CURRENT_SOURCE_DIR}"
    CLASSPATH ${CMAKE_JAVA_INCLUDE_PATH}
    WINDOWTITLE "JarSearch documentation"
    DOCTITLE "<h1>JarSearch documentation</h1>"
    AUTHOR TRUE
    USE TRUE
    VERSION TRUE
    )
endif()
