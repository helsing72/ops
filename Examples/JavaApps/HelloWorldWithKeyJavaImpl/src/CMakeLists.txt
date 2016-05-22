
# generate a MANIFEST file
configure_file(MANIFEST.MF.in MANIFEST.MF)

set(APP_SRC
  helloworldjavaimpl/MainPublish.java
)

add_jar(MainPublishWithKey ${APP_SRC}
  INCLUDE_JARS ${PROJECT_BINARY_DIR}/bin/HelloWorld.jar OPSJLib ConfigurationLib
  ENTRY_POINT helloworldjavaimpl/MainPublish
  MANIFEST ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)
add_dependencies(MainPublishWithKey HelloWorld)

set(APP_SRC
  helloworldjavaimpl/MainSubscribe.java
)

add_jar(MainSubscribeWithKey ${APP_SRC}
  INCLUDE_JARS ${PROJECT_BINARY_DIR}/bin/HelloWorld.jar OPSJLib ConfigurationLib
  ENTRY_POINT helloworldjavaimpl/MainSubscribe
  MANIFEST ${CMAKE_CURRENT_BINARY_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)
add_dependencies(MainSubscribeWithKey HelloWorld)

install(FILES
  ${CMAKE_BINARY_DIR}/bin/MainPublishWithKey-${PROJECT_VERSION}.jar
  ${CMAKE_BINARY_DIR}/bin/MainSubscribeWithKey-${PROJECT_VERSION}.jar
  ${CMAKE_BINARY_DIR}/bin/OPSJLib-${PROJECT_VERSION}.jar
  ${CMAKE_BINARY_DIR}/bin/ConfigurationLib-${PROJECT_VERSION}.jar
  ${PROJECT_BINARY_DIR}/bin/HelloWorld.jar
  ../ops_config.xml
  DESTINATION examples/helloworld
)
