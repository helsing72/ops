
find_program(CSCOMPILER NAMES csc.exe gmcs mcs
  HINTS
    "C:/Program Files (x86)/MSBuild/14.0"
    C:/Windows/Microsoft.NET/Framework/v4.0.30319
    /sw/tools/mono/2.6.1
  PATH_SUFFIXES bin
  )
if(CSCOMPILER)

  # add the source
  add_subdirectory(Ops)
endif()
