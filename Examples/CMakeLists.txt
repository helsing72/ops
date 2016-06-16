
# Generate code for Example idl projects
add_subdirectory(OPSIdls)

# Define path to the OPSIDL directories that we need in subdirectories
set(OPSIDL_SOURCE_DIR ${CMAKE_SOURCE_DIR}/Examples/OPSIdls)

# Java example applications
add_subdirectory(JavaApps)

# C++ example applications
# All will need the ops headers
include_directories(${CMAKE_SOURCE_DIR}/Cpp/include)

add_subdirectory(CppApps)
