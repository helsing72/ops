
if(MINGW)
  message(STATUS "No support for CSharp when building with MinGW toolchain yet")
  return()
endif()

if(UNIX)
  message(STATUS "No support for CSharp when building with Mono toolchain yet")
  return()
endif()

if(CMAKE_CL_64)
  find_program(CSCOMPILER NAMES csc.exe gmcs mcs
    HINTS
      $ENV{WINDIR}/Microsoft.NET/Framework64/v4.0.30319
      #$ENV{WINDIR}/Microsoft.NET/Framework64/v3.5
      /sw/tools/mono/2.6.1
    PATH_SUFFIXES bin
    )
else()
  find_program(CSCOMPILER NAMES csc.exe gmcs mcs
    HINTS
      $ENV{WINDIR}/Microsoft.NET/Framework/v4.0.30319
      #$ENV{WINDIR}/Microsoft.NET/Framework/v3.5
      /sw/tools/mono/2.6.1
    PATH_SUFFIXES bin
    )
endif()

if(CSCOMPILER)
  # add the source
  add_subdirectory(Ops)
else()
  message(STATUS "CSharp compiler not found...")
endif()
