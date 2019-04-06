
set(BUILD_NAME opsbridgeidls)

# list of source files that, when changed, require regeneration of target
set(IDL_SRCS
    ${CMAKE_CURRENT_SOURCE_DIR}/OpsBridgeCommandData.idl
    ${CMAKE_CURRENT_SOURCE_DIR}/OpsBridgeStatusData.idl
    ${CMAKE_CURRENT_SOURCE_DIR}/TestBlockData.idl
    )

# the output files that we want are several, but we use one for the check
set(WANTED_OUTPUT ${CMAKE_CURRENT_SOURCE_DIR}/Generated/Cpp/opsbridge/OpsBridgeCommandData.h)

# use opsc script from build tree, will be used in all subdirs under this one
if(WIN32)
  set(OPS_COMPILER ${CMAKE_BINARY_DIR}/bin/opsc.bat)
else()
  set(OPS_COMPILER ${CMAKE_BINARY_DIR}/bin/opsc.sh)
endif()

add_custom_command(OUTPUT ${WANTED_OUTPUT}
  COMMAND ${OPS_COMPILER} -g ALL -B ALL -o ${CMAKE_CURRENT_SOURCE_DIR}/Generated -p opsbridge ${IDL_SRCS}
  DEPENDS ${IDL_SRCS}
  COMMENT "Generating source files from IDL's in ${CMAKE_CURRENT_SOURCE_DIR}"
  VERBATIM
  )

add_custom_target(${BUILD_NAME} DEPENDS ${WANTED_OUTPUT})
add_dependencies(${BUILD_NAME} opsc)
