
# the library name
if(CMAKE_BUILD_TYPE MATCHES "Release")
	set(LIB_NAME opsopt)
elseif(CMAKE_BUILD_TYPE MATCHES "Debug")
	set(LIB_NAME opsdbg)
endif()	


# where are the headers
set(HEADER_PATH ${PROJECT_SOURCE_DIR}/Cpp/include)

set(LIB_PUBLIC_HEADERS
  ${HEADER_PATH}/ArchiverInOut.h
  ${HEADER_PATH}/ByteBuffer.h
  ${HEADER_PATH}/ByteListener.h
  ${HEADER_PATH}/BytesSizePair.h
  ${HEADER_PATH}/CommException.h
  ${HEADER_PATH}/Compatibility.h
  ${HEADER_PATH}/ConfigException.h
  ${HEADER_PATH}/ConfigLoader.h
  ${HEADER_PATH}/DataListener.h
  ${HEADER_PATH}/DataNotifier.h
  ${HEADER_PATH}/DeadlineMissedListener.h
  ${HEADER_PATH}/DeadlineTimer.h
  ${HEADER_PATH}/Domain.h
  ${HEADER_PATH}/Error.h
  ${HEADER_PATH}/ErrorService.h
  ${HEADER_PATH}/ErrorWriter.h
  ${HEADER_PATH}/FilterQoSPolicy.h
  ${HEADER_PATH}/IOService.h
  ${HEADER_PATH}/KeyFilterQoSPolicy.h
  ${HEADER_PATH}/Listener.h
  ${HEADER_PATH}/Lockable.h
  ${HEADER_PATH}/MemoryMap.h
  ${HEADER_PATH}/NoSuchTopicException.h
  ${HEADER_PATH}/Notifier.h
  ${HEADER_PATH}/ops/Reply.h
  ${HEADER_PATH}/ops/Request.h
  ${HEADER_PATH}/OPSArchiverIn.h
  ${HEADER_PATH}/OPSArchiverOut.h
  ${HEADER_PATH}/OPSConfig.h
  ${HEADER_PATH}/OPSConstants.h
  ${HEADER_PATH}/ops.h
  ${HEADER_PATH}/OPSMessage.h
  ${HEADER_PATH}/OPSObjectFactory.h
  ${HEADER_PATH}/OPSObject.h
  ${HEADER_PATH}/OPSTypeDefs.h
  ${HEADER_PATH}/Participant.h
  ${HEADER_PATH}/ParticipantInfoData.h
  ${HEADER_PATH}/ParticipantInfoDataListener.h
  ${HEADER_PATH}/PrintArchiverOut.h
  ${HEADER_PATH}/Publisher.h
  ${HEADER_PATH}/ReceiveDataHandler.h
  ${HEADER_PATH}/Receiver.h
  ${HEADER_PATH}/ReceiveTransport.h
  ${HEADER_PATH}/ReferenceHandler.h
  ${HEADER_PATH}/RequestReply.h
  ${HEADER_PATH}/Reservable.h
  ${HEADER_PATH}/ReserveInfo.h
  ${HEADER_PATH}/Runnable.h
  ${HEADER_PATH}/SendDataHandler.h
  ${HEADER_PATH}/Sender.h
  ${HEADER_PATH}/SendTransport.h
  ${HEADER_PATH}/SerializableCompositeFactory.h
  ${HEADER_PATH}/SerializableFactory.h
  ${HEADER_PATH}/SerializableInheritingTypeFactory.h
  ${HEADER_PATH}/Serializable.h
  ${HEADER_PATH}/SingleThreadPool.h
  ${HEADER_PATH}/Subscriber.h
  ${HEADER_PATH}/Thread.h
  ${HEADER_PATH}/ThreadPool.h
  ${HEADER_PATH}/TimeHelper.h
  ${HEADER_PATH}/Topic.h
  ${HEADER_PATH}/TopicInfoData.h
  ${HEADER_PATH}/Transaction.h
  ${HEADER_PATH}/xml/xmlParser.h
  ${HEADER_PATH}/XMLArchiverIn.h
  ${HEADER_PATH}/XMLArchiverOut.h
)

set(LIB_SRCS
  BasicError.h
  BoostDeadlineTimerImpl.h
  BoostIOServiceImpl.h
  ByteBuffer.cpp
  Compatibility.cpp
  DataNotifier.cpp
  DeadlineTimer.cpp
  DefaultOPSConfigImpl.h
  Domain.cpp
  ErrorService.cpp
  IOService.cpp
  KeyFilterQoSPolicy.cpp
  Lockable.cpp
  McSendDataHandler.h
  McUdpSendDataHandler.h
  MulticastReceiver.cpp
  MulticastReceiver.h
  MultiThreadPool.h
  OPSArchiverIn.cpp
  OPSArchiverOut.cpp
  OPSConfig.cpp
  OPSObject.cpp
  OPSObjectFactory.cpp
  OPSObjectFactoryImpl.h
  Participant.cpp
  ParticipantInfoDataListener.cpp
  Publisher.cpp
  ReceiveDataHandler.cpp
  ReceiveDataHandlerFactory.cpp
  ReceiveDataHandlerFactory.h
  Receiver.cpp
  ReceiverFactory.cpp
  ReceiverFactory.h
  RequestReply.cpp
  Reservable.cpp
  SendDataHandlerFactory.cpp
  SendDataHandlerFactory.h
  SendDiscoveryModule.cpp
  Sender.cpp
  SerializableCompositeFactory.cpp
  SerializableInheritingTypeFactory.cpp
  Subscriber.cpp
  TCPClient.cpp
  TCPClient.h
  TCPSendDataHandler.h
  TCPServer.h
  Thread.cpp
  TimeHelper.cpp
  Topic.cpp
  UDPReceiver.cpp
  UDPReceiver.h
  UDPSender.cpp
  UDPSender.h
  xml/xmlParser.cpp
  XMLArchiverIn.cpp
  XMLArchiverOut.cpp
)

# ide grouping, doesn't matter for makefile projects
source_group("Header Files" FILES ${LIB_PUBLIC_HEADERS})

# generate the library target
add_library(${LIB_NAME}
    ${LIB_SRCS}
    ${LIB_PUBLIC_HEADERS}
)


set_target_properties(${LIB_NAME} PROPERTIES VERSION ${PROJECT_VERSION})

target_link_libraries(${LIB_NAME} ${Boost_LIBRARIES})

# library and headers installation
set(INSTALL_INCDIR include)
set(INSTALL_BINDIR bin)
if(WIN32 AND NOT CYGWIN)
  set(INSTALL_LIBDIR bin)
  set(INSTALL_ARCHIVEDIR lib)
else()
  set(INSTALL_LIBDIR lib${LIB_POSTFIX})
  set(INSTALL_ARCHIVEDIR lib${LIB_POSTFIX})
endif()

install(TARGETS ${LIB_NAME}
  RUNTIME DESTINATION ${INSTALL_BINDIR}
  LIBRARY DESTINATION ${INSTALL_LIBDIR}
  ARCHIVE DESTINATION ${INSTALL_ARCHIVEDIR}
  )


# install headers separately to keep the include file hierarchy
install(DIRECTORY ${HEADER_PATH}/ DESTINATION ${INSTALL_INCDIR}/${LIB_NAME})
