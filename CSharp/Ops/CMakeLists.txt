
set(OPS_LIB_NAME ops-csharp)

set(OPS_CS_SOURCE
  AckData.cs
  BlockingBuffer.cs
  DeadlineMissedFilterQoSPolicy.cs
  DeadlineNotifier.cs
  DefaultOPSConfigImpl.cs
  DefaultSubscriberFilterQoSPolicy.cs
  Domain.cs
  Error.cs
  Event.cs
  Exceptions.cs
  FilterQoSPolicy.cs
  Globals.cs
  InetAddress.cs
  InProcessTransport.cs
  KeyFilterQoSPolicy.cs
  Listener.cs
  Locals.cs
  McSendDataHandler.cs
  McUdpSendDataHandler.cs
  MessageFilter.cs
  MessageFilterSet.cs
  MulticastReceiver.cs
  MulticastSender.cs
  Notifier.cs
  OPSConfig.cs
  OPSMessage.cs
  OPSObject.cs
  Participant.cs
  ParticipantFactory.cs
  ParticipantInfoData.cs
  ParticipantInfoDataListener.cs
  Publisher.cs
  QualityOfService.cs
  QualityOfServicePolicy.cs
  ReadByteBuffer.cs
  ReceiveDataHandler.cs
  ReceiveDataHandlerFactory.cs
  Receiver.cs
  ReceiverFactory.cs
  ReliabilityQoS.cs
  Reply.cs
  Request.cs
  RequestReply.cs
  SendDataHandler.cs
  SendDataHandlerFactory.cs
  Sender.cs
  Subscriber.cs
  SubscriberFilterQoSPolicy.cs
  TcpClient.cs
  TcpClientReceiver.cs
  TcpSendDataHandler.cs
  TcpSenderList.cs
  TcpServerSender.cs
  Topic.cs
  TopicInfoData.cs
  UdpReceiver.cs
  UdpSender.cs
  WriteByteBuffer.cs
  Archiver/ArchiverInOut.cs
  Archiver/FormatException.cs
  Archiver/OPSArchiverIn.cs
  Archiver/OPSArchiverOut.cs
  Archiver/OPSObjectFactory.cs
  Archiver/SerializableCompositeFactory.cs
  Archiver/Serializable.cs
  Archiver/SerializableFactory.cs
  Archiver/XMLArchiverIn.cs
  Archiver/XMLArchiverOut.cs
  Utilities/EasyThread.cs
  Utilities/EmailLogger.cs
  Utilities/EventLogLogger.cs
  Utilities/ExceptionLogger.cs
  Utilities/Logger.cs
  Utilities/ObserverPattern.cs
  Utilities/TextFileLogger.cs
  Utilities/WebsiteLogger.cs
  Utilities/WindowLogger.cs
)

set(OPS_DBG_TARGET ${CMAKE_BINARY_DIR}/bin/${OPS_LIB_NAME}-${OPS_VERSION}-dbg.dll)
set(OPS_OPT_TARGET ${CMAKE_BINARY_DIR}/bin/${OPS_LIB_NAME}-${OPS_VERSION}-opt.dll)


set(OPS_TARGET ${OPS_OPT_TARGET}) #pane, added to OPS_OPT_TARGET ...


if(MSVC)
  # message("CSCOMPILER ${CSCOMPILER}")

  # find_path(CSREFERENCE_PATH System.Data.dll HINTS ${CSCOMPILER})
  find_path(CSREFERENCE_PATH System.Data.dll HINTS "C:\\Program Files (x86)\\Reference Assemblies\\Microsoft\\Framework\\.NETFramework\\v4.6")
  file(TO_NATIVE_PATH ${CSREFERENCE_PATH} CSREFERENCE_NATIVEPATH)

  # message("CSREFERENCE_PATH ${CSREFERENCE_PATH}")

  # convert files to native file paths, i.e. with '\'
  foreach(csfile ${OPS_CS_SOURCE})
    file(TO_NATIVE_PATH ${csfile} CSNATIVEFILE)
    list(APPEND OPS_CS_SOURCE_LIST ${CSNATIVEFILE})
  endforeach()

  add_custom_command(OUTPUT ${OPS_DBG_TARGET} ${OPS_OPT_TARGET}
    COMMAND "C:\\Program Files (x86)\\Microsoft Visual Studio 14.0\\VC\\vcvarsall.bat" x86
    COMMAND ${CSCOMPILER} /noconfig /debug /nowarn:1701,1702 /warn:4 /define:TRACE /lib:${CSREFERENCE_NATIVEPATH} /reference:System.dll,System.Drawing.dll,System.Windows.Forms.dll,System.Xml.dll,System.Linq.dll,System.Web.dll,System.Web.Services.dll /target:library /out:${OPS_DBG_TARGET} ${OPS_CS_SOURCE_LIST}
    COMMAND ${CSCOMPILER} /noconfig /optimize /nowarn:1701,1702 /warn:4 /define:TRACE /lib:${CSREFERENCE_NATIVEPATH} /reference:System.dll,System.Drawing.dll,System.Windows.Forms.dll,System.Xml.dll,System.Linq.dll,System.Web.dll,System.Web.Services.dll /target:library /out:${OPS_OPT_TARGET} ${OPS_CS_SOURCE_LIST}
    DEPENDS ${OPS_CS_SOURCE}
    COMMENT "Building C# sources using ${CSCOMPILER}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    VERBATIM
    )
else()
  # removed System.Net.dll for Microsoft.Net v2.0.xyz
  add_custom_command(OUTPUT ${OPS_TARGET}  
	COMMAND ${CSCOMPILER} -reference:System.dll,System.Drawing.dll,System.Windows.Forms.dll,System.Web.dll,System.Web.Services.dll -target:library -out:${OPS_TARGET} ${OPS_CS_SOURCE}
    DEPENDS ${OPS_CS_SOURCE}
    COMMENT "Building C# sources using ${CSCOMPILER}"
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    VERBATIM
    )
endif()

add_custom_target(ops-csharp ALL DEPENDS ${OPS_DBG_TARGET} ${OPS_OPT_TARGET})

install(FILES ${OPS_DBG_TARGET} ${OPS_OPT_TARGET} DESTINATION lib/mono)
