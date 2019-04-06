
# the Exe name
set(EXE_NAME OPSBridge)


set(EXE_SRCS
BridgeConfig.cpp
BridgeConfig.h
BridgeLog.h
BridgeMessages.h
BridgeTypeFactory.h
CBridge.cpp
CBridge.h
Configuration.h
CSocketTransport.cpp
CSocketTransport.h
CTcpTransportClient.cpp
CTcpTransportClient.h
CTcpTransportServer.cpp
CTcpTransportServer.h
CTransport.cpp
CTransport.h
CWorkerThread.cpp
CWorkerThread.h
OpsBridge.cpp
RawMcUdp.cpp
RawMcUdp.h
SEHException.cpp
SEHException.h
)

# generate the library target
add_executable(${EXE_NAME}
    ${EXE_SRCS}
)
add_dependencies(${EXE_NAME} opsbridgeidls)

target_link_libraries(${EXE_NAME} ${Boost_LIBRARIES} ops-static)

if(WIN32)
  target_link_libraries(${EXE_NAME} winmm psapi)
  if(MINGW)
    target_link_libraries(${EXE_NAME} ws2_32 mswsock)
  endif()
else()
  target_link_libraries(${EXE_NAME} pthread rt)
endif()

# exe installation
set(INSTALL_BINDIR bin)

install(TARGETS ${EXE_NAME}
  RUNTIME DESTINATION ${INSTALL_BINDIR}
  )
