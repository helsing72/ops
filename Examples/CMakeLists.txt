
# generate code from Example idl projects
add_subdirectory(OPSIdls)

set(OPSIDL_SOURCE_DIR ${CMAKE_SOURCE_DIR}/Examples/OPSIdls)

# Java example applications
add_subdirectory(JavaApps)

# C++ example applications
# All will need the ops headers
include_directories(${CMAKE_SOURCE_DIR}/Cpp/include)

add_subdirectory(HelloRequesterCppImpl)

add_subdirectory(HelloWorldCppImpl)
add_subdirectory(HelloWorldWithKeyCppImpl)

add_subdirectory(VC++Apps/PizzaTest)
