
set(APP_SRC
  pizzatest/Main.java
)

add_jar(PizzaTest ${APP_SRC}
  INCLUDE_JARS ${PROJECT_BINARY_DIR}/bin/PizzaProject.jar OPSJLib ConfigurationLib
  ENTRY_POINT pizzatest/Main
  MANIFEST ${CMAKE_CURRENT_SOURCE_DIR}/MANIFEST.MF
  VERSION ${PROJECT_VERSION}
  OUTPUT_DIR ${CMAKE_BINARY_DIR}/bin
)
add_dependencies(PizzaTest PizzaProject)
