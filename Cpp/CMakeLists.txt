
# Make the headers visible to everything
include_directories(
  ${CMAKE_CURRENT_SOURCE_DIR}/include
)

################################################################################
# Find external dependencies

# if we need to link with libraries
include_directories(${Boost_INCLUDE_DIRS})

################################################################################
# Build the source

# add the source
add_subdirectory(source)

################################################################################
# Documentation

# Find doxygen, dot, latex, htmlhelp
#sbd_setup_documentation()

# Do documentation targets
if(BUILD_DOCUMENTATION AND DOXYGEN)
  # This processes our *doxyfile.in template
  configure_file(${PROJECT_SOURCE_DIR}/Cpp/ops.doxyfile.in
    ${PROJECT_BINARY_DIR}/doc/ops/ops.doxyfile
    )
  # copy image
  #sbd_install_logo( ${PROJECT_BINARY_DIR}/doc/nsc/html )

  # This creates a new target to build documentation.
  add_custom_target("doc-ops" ${DOXYGEN}
    ${PROJECT_BINARY_DIR}/doc/ops/ops.doxyfile
    )

  # install html documentation
  install(DIRECTORY ${PROJECT_BINARY_DIR}/doc/ops/html
    DESTINATION doc/ops
    COMPONENT ops_doc
  )

endif()
