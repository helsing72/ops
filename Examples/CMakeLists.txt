
# generate code from Example idl projects
add_subdirectory(OPSIdls)

# java example applications
add_subdirectory(JavaApps)

##add_subdirectory(PizzaTest/src)

# C++ applications

# All will need the ops headers
include_directories(${CMAKE_SOURCE_DIR}/Cpp/include)

add_subdirectory(HelloWorldCppImpl)

add_subdirectory(VC++Apps/PizzaTest)
