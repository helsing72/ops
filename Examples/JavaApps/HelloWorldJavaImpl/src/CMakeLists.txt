
set(APP_SRC
  helloworldjavaimpl/MainPublish.java
)

add_jar(MainPublish ${APP_SRC}
  INCLUDE_JARS ${PROJECT_BINARY_DIR}/bin/HelloWorld.jar OPSJLib ConfigurationLib
  ENTRY_POINT helloworldjavaimpl/MainPublish
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)
add_dependencies(MainPublish HelloWorld)

set(APP_SRC
  helloworldjavaimpl/MainSubscribe.java
)

add_jar(MainSubscribe ${APP_SRC}
  INCLUDE_JARS ${PROJECT_BINARY_DIR}/bin/HelloWorld.jar OPSJLib ConfigurationLib
  ENTRY_POINT helloworldjavaimpl/MainSubscribe
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)
add_dependencies(MainSubscribe HelloWorld)
