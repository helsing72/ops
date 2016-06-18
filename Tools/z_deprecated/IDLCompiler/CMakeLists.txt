
# IDLCompiler needs OPSComilerLib
set(CMAKE_JAVA_INCLUDE_PATH ${CMAKE_JAVA_INCLUDE_PATH}
  ${ANTLR_JAR}
  ${JAVACC_JAR}
  ${JAXMEJS_JAR}
  ${SWINGLAYOUT_JAR}
)

add_subdirectory(src)
