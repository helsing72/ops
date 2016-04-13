
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

# if we need to link with libraries
find_package(Java COMPONENTS Runtime Development)

if(Java_Runtime_FOUND)
  message(STATUS "Found Java version: ${Java_VERSION}")
  include(UseJava)
endif()

if(NOT Java_JAVAC_EXECUTABLE)
  message(WARNING "Could not find a Java compiler")
endif()

# find Boost. If not found we can't build c++ libs
set(Boost_NO_BOOST_CMAKE ON)
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

# Dynamic vs Static Linking
option(OPS_DYNAMIC "Set to ON to build OPS for dynamic linking.  Use OFF for static." ON)
if(OPS_DYNAMIC)
  set(BUILD_SHARED_LIBS ON)
else()
  set(BUILD_SHARED_LIBS OFF)
endif()

# set path to where jars should be installed
set(OPS_JAR_REL_INSTALL_PATH lib/ops)

################################################################################
# Build the source

if(Java_JAVAC_EXECUTABLE)
  # this builds ConfigurationLib and JarSearch
  add_subdirectory(Libs)
  # build OPSJLib
  add_subdirectory(Java)
  # build the rest
  add_subdirectory(Tools)
endif()

add_subdirectory(CSharp)

# add the C++ source
if(Boost_FOUND)
  add_subdirectory(Cpp)
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
