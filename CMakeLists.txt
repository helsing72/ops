
cmake_minimum_required(VERSION 3.0.0 FATAL_ERROR)

# The main project name
project(OPS VERSION 4.1.0)

################################################################################
#
#if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
if (WIN32)
  set (CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/installed" CACHE PATH "default install path" FORCE )
endif()

################################################################################
# Find external dependencies

# help the FindGtest script locate gtest
if(DEFINED ENV{GOOGLETEST_HOME})
  set(GTEST_ROOT $ENV{GOOGLETEST_HOME})
  list(APPEND CMAKE_PREFIX_PATH $ENV{GOOGLETEST_LIB})
endif()
find_package(GTest)

# if we need to link with libraries
find_package(Java COMPONENTS Runtime Development)

if(Java_Runtime_FOUND)
  message(STATUS "Found Java version: ${Java_VERSION}")
  include(UseJava)
endif()

if(NOT Java_JAVAC_EXECUTABLE)
  message(WARNING "Could not find a Java compiler")
endif()


# add compiler flags for release or debug
if(MSVC)
#  set(warnings "/W4 /WX /EHsc")
  set(warnings "/W3 /WX /EHsc")
else()
  if(CMAKE_BUILD_TYPE MATCHES "Release" OR CMAKE_BUILD_TYPE MATCHES "")
  	set(CMAKE_CXX_FLAGS "-O2")
  	string(REPLACE "-O3 -DNDEBUG" "" CMAKE_CXX_FLAGS_RELEASE ${CMAKE_CXX_FLAGS_RELEASE} )
  elseif(CMAKE_BUILD_TYPE MATCHES "Debug")
  	set(CMAKE_CXX_FLAGS "-g -fprofile-arcs -ftest-coverage -O0") #fPIC is set
  	string(REPLACE "-g" "" CMAKE_CXX_FLAGS_DEBUG ${CMAKE_CXX_FLAGS_DEBUG} )
  endif()
#  set(warnings "-Wall -Wextra -Werror")
  set(warnings "-Wall")
endif()
set(CMAKE_C_FLAGS   "${CMAKE_C_FLAGS} ${warnings}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${warnings}")

message ("c Flags: " ${CMAKE_C_FLAGS})
message ("cxx Flags: " ${CMAKE_CXX_FLAGS})

# find Boost. If not found we can't build c++ libs
option(Boost_DEBUG "Set to ON to debug Boost paths" OFF)
# this prevents the FindBoost.cmake to search for cmake-built Boost
set(Boost_NO_BOOST_CMAKE ON)
# if the Boost home environment var is set, use it to hint the FindBoost.cmake about Boost location
set(BOOST_ROOT $ENV{BOOST_HOME})

if(MSVC)
  set(Boost_USE_STATIC_LIBS ON)
  find_package(Boost MODULE COMPONENTS system thread date_time regex chrono)
else()
  find_package(Boost MODULE COMPONENTS system thread)
endif()


# If we have java
# we need ANother Tool for Language Recognition
find_jar(ANTLR_JAR antlr
  PATHS Tools/IDLCompiler/dist/lib
# [VERSIONS version1 [version2]]
  DOC "Path to antlr.jar"
  )
# Find "Java Compiler Compiler" archive
find_jar(JAVACC_JAR javacc
  PATHS Tools/IDLCompiler/dist/lib
  DOC "Path to javacc.jar"
  )
# Find jaxmejs. JaxMe is archive and is not maintained!!!
find_jar(JAXMEJS_JAR jaxmejs
  PATHS Tools/IDLCompiler/dist/lib
  VERSIONS 0.5.2
  DOC "Path to jaxmejs.jar"
  )
# Find swing-layout.jar. This is anextension to swing required by NetBeans
find_jar(SWINGLAYOUT_JAR swing-layout
  PATHS Tools/IDLCompiler/dist/lib
#  VERSIONS 1.0.4
  DOC "Path to swing-layout jar"
  )

################################################################################
# User Options

include(${CMAKE_ROOT}/Modules/Documentation.cmake OPTIONAL)
mark_as_advanced(CLEAR BUILD_DOCUMENTATION)

# set path to where jars should be installed
set(OPS_JAR_REL_INSTALL_PATH lib/ops)

################################################################################
# Build the source

# recurse into java source
if(Java_JAVAC_EXECUTABLE)
  # this builds ConfigurationLib and JarSearch
  add_subdirectory(Libs)
  # build OPSJLib
  add_subdirectory(Java)
  # build the rest
  add_subdirectory(Tools)
endif()


add_subdirectory(CSharp)

# add the C++ source, only if Boost is loaded
if(Boost_FOUND)
  add_subdirectory(Cpp)

  option(OPS_BUILD_UNITTESTS "Enable to build the UnitTest app(s)" OFF)
  if(OPS_BUILD_UNITTESTS AND CMAKE_BUILD_TYPE MATCHES "Debug")
    add_subdirectory(UnitTests) #pane added
  endif()

else()
  message(WARNING "install Boost to build OPS C++ libraries")
endif()

################################################################################
# Build Examples


################################################################################
# present the packaging option only if we have the cpack command defined (effectively >= 2.6.0)
if(CMAKE_CPACK_COMMAND)
    option(OPS_BUILD_PACKAGES "Set to ON to generate CPack configuration files and packaging targets" OFF)
    if(OPS_BUILD_PACKAGES)
        set(CPACK_PACKAGE_NAME "libops")
        set(CPACK_DEBIAN_PACKAGE_DEPENDS "libc6, libgcc1, libstdc++6")
        set(CPACK_PACKAGE_DESCRIPTION_SUMMARY "Open Publish-Subscribe communication library")
        set(CPACK_PACKAGE_DESCRIPTION "A LONG DESCRIPTION")
        set(CPACK_PACKAGE_VENDOR "Saab Dynamics AB")
        set(CPACK_PACKAGE_CONTACT "Anton???")
        set(CPACK_PACKAGE_VERSION_MAJOR ${PROJECT_MAJOR_VERSION})
        set(CPACK_PACKAGE_VERSION_MINOR ${PROJECT_MINOR_VERSION})
        set(CPACK_PACKAGE_VERSION_PATCH ${PROJECT_PATCH_VERSION})
        set(CPACK_PACKAGE_INSTALL_DIRECTORY "${CPACK_PACKAGE_NAME} ${PROJECT_VERSION}")
        set(CPACK_RESOURCE_FILE_LICENSE "${PROJECT_SOURCE_DIR}/COPYING")
        if(WIN32)
          set(CPACK_GENERATOR NSIS ZIP)
          set(CPACK_SOURCE_GENERATOR ZIP)
        else()
          set(CPACK_GENERATOR TGZ)
          set(CPACK_SOURCE_GENERATOR TGZ)
        endif()
        set(CPACK_NSIS_MODIFY_PATH OFF)

        # disable ALL components
        set(CPACK_COMPONENT_OPS_DEV_DISABLED TRUE)
        set(CPACK_COMPONENT_TESTS_DISABLED TRUE)

        include(CPack)
    endif()
endif()
