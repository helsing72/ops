#
# Generated Makefile - do not edit!
#
# Edit the Makefile in the project folder instead (../Makefile). Each target
# has a -pre and a -post target defined where you can add customized code.
#
# This makefile implements configuration specific macros and targets.


# Environment
MKDIR=mkdir
CP=cp
CCADMIN=CCadmin
RANLIB=ranlib
CC=gcc
CCC=g++
CXX=g++
FC=
AS=as

# Macros
CND_PLATFORM=GNU-Linux-x86
CND_CONF=Debug
CND_DISTDIR=dist

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=build/${CND_CONF}/${CND_PLATFORM}

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/MulticastReceiver.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/UDPSender.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Publisher.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Lockable.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/SerializableCompositeFactory.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/DeadlineTimer.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ParticipantInfoDataListener.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/XMLArchiverOut.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Domain.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/XMLArchiverIn.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSArchiverIn.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Participant.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/IOService.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ReceiveDataHandler.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Thread.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/TimeHelper.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSArchiverOut.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Receiver.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/RequestReply.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ErrorService.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSObjectFactory.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ReceiverFactory.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/DataNotifier.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSObject.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ByteBuffer.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ReceiveDataHandlerFactory.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/SendDataHandlerFactory.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/TCPClient.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSConfig.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Reservable.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Subscriber.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Topic.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/KeyFilterQoSPolicy.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Sender.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/xml/xmlParser.o \
	${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/UDPReceiver.o

# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=-pthread
CXXFLAGS=-pthread

# Fortran Compiler Flags
FFLAGS=

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	${MAKE}  -f nbproject/Makefile-Debug.mk dist/Debug/GNU-Linux-x86/libopscpplinuxlib.a

dist/Debug/GNU-Linux-x86/libopscpplinuxlib.a: ${OBJECTFILES}
	${MKDIR} -p dist/Debug/GNU-Linux-x86
	${RM} dist/Debug/GNU-Linux-x86/libopscpplinuxlib.a
	${AR} rv ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/libopscpplinuxlib.a ${OBJECTFILES} 
	$(RANLIB) dist/Debug/GNU-Linux-x86/libopscpplinuxlib.a

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/MulticastReceiver.o: nbproject/Makefile-${CND_CONF}.mk ../../source/MulticastReceiver.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/MulticastReceiver.o ../../source/MulticastReceiver.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/UDPSender.o: nbproject/Makefile-${CND_CONF}.mk ../../source/UDPSender.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/UDPSender.o ../../source/UDPSender.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Publisher.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Publisher.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Publisher.o ../../source/Publisher.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Lockable.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Lockable.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Lockable.o ../../source/Lockable.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/SerializableCompositeFactory.o: nbproject/Makefile-${CND_CONF}.mk ../../source/SerializableCompositeFactory.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/SerializableCompositeFactory.o ../../source/SerializableCompositeFactory.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/DeadlineTimer.o: nbproject/Makefile-${CND_CONF}.mk ../../source/DeadlineTimer.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/DeadlineTimer.o ../../source/DeadlineTimer.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ParticipantInfoDataListener.o: nbproject/Makefile-${CND_CONF}.mk ../../source/ParticipantInfoDataListener.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ParticipantInfoDataListener.o ../../source/ParticipantInfoDataListener.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/XMLArchiverOut.o: nbproject/Makefile-${CND_CONF}.mk ../../source/XMLArchiverOut.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/XMLArchiverOut.o ../../source/XMLArchiverOut.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Domain.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Domain.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Domain.o ../../source/Domain.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/XMLArchiverIn.o: nbproject/Makefile-${CND_CONF}.mk ../../source/XMLArchiverIn.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/XMLArchiverIn.o ../../source/XMLArchiverIn.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSArchiverIn.o: nbproject/Makefile-${CND_CONF}.mk ../../source/OPSArchiverIn.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSArchiverIn.o ../../source/OPSArchiverIn.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Participant.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Participant.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Participant.o ../../source/Participant.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/IOService.o: nbproject/Makefile-${CND_CONF}.mk ../../source/IOService.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/IOService.o ../../source/IOService.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ReceiveDataHandler.o: nbproject/Makefile-${CND_CONF}.mk ../../source/ReceiveDataHandler.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ReceiveDataHandler.o ../../source/ReceiveDataHandler.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Thread.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Thread.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Thread.o ../../source/Thread.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/TimeHelper.o: nbproject/Makefile-${CND_CONF}.mk ../../source/TimeHelper.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/TimeHelper.o ../../source/TimeHelper.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSArchiverOut.o: nbproject/Makefile-${CND_CONF}.mk ../../source/OPSArchiverOut.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSArchiverOut.o ../../source/OPSArchiverOut.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Receiver.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Receiver.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Receiver.o ../../source/Receiver.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/RequestReply.o: nbproject/Makefile-${CND_CONF}.mk ../../source/RequestReply.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/RequestReply.o ../../source/RequestReply.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ErrorService.o: nbproject/Makefile-${CND_CONF}.mk ../../source/ErrorService.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ErrorService.o ../../source/ErrorService.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSObjectFactory.o: nbproject/Makefile-${CND_CONF}.mk ../../source/OPSObjectFactory.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSObjectFactory.o ../../source/OPSObjectFactory.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ReceiverFactory.o: nbproject/Makefile-${CND_CONF}.mk ../../source/ReceiverFactory.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ReceiverFactory.o ../../source/ReceiverFactory.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/DataNotifier.o: nbproject/Makefile-${CND_CONF}.mk ../../source/DataNotifier.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/DataNotifier.o ../../source/DataNotifier.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSObject.o: nbproject/Makefile-${CND_CONF}.mk ../../source/OPSObject.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSObject.o ../../source/OPSObject.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ByteBuffer.o: nbproject/Makefile-${CND_CONF}.mk ../../source/ByteBuffer.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ByteBuffer.o ../../source/ByteBuffer.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ReceiveDataHandlerFactory.o: nbproject/Makefile-${CND_CONF}.mk ../../source/ReceiveDataHandlerFactory.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/ReceiveDataHandlerFactory.o ../../source/ReceiveDataHandlerFactory.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/SendDataHandlerFactory.o: nbproject/Makefile-${CND_CONF}.mk ../../source/SendDataHandlerFactory.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/SendDataHandlerFactory.o ../../source/SendDataHandlerFactory.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/TCPClient.o: nbproject/Makefile-${CND_CONF}.mk ../../source/TCPClient.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/TCPClient.o ../../source/TCPClient.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSConfig.o: nbproject/Makefile-${CND_CONF}.mk ../../source/OPSConfig.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/OPSConfig.o ../../source/OPSConfig.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Reservable.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Reservable.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Reservable.o ../../source/Reservable.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Subscriber.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Subscriber.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Subscriber.o ../../source/Subscriber.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Topic.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Topic.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Topic.o ../../source/Topic.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/KeyFilterQoSPolicy.o: nbproject/Makefile-${CND_CONF}.mk ../../source/KeyFilterQoSPolicy.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/KeyFilterQoSPolicy.o ../../source/KeyFilterQoSPolicy.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Sender.o: nbproject/Makefile-${CND_CONF}.mk ../../source/Sender.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/Sender.o ../../source/Sender.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/xml/xmlParser.o: nbproject/Makefile-${CND_CONF}.mk ../../source/xml/xmlParser.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/xml
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/xml/xmlParser.o ../../source/xml/xmlParser.cpp

${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/UDPReceiver.o: nbproject/Makefile-${CND_CONF}.mk ../../source/UDPReceiver.cpp 
	${MKDIR} -p ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source
	${RM} $@.d
	$(COMPILE.cc) -g -I../../include -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/_ext/_DOTDOT/_DOTDOT/source/UDPReceiver.o ../../source/UDPReceiver.cpp

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf:
	${RM} -r build/Debug
	${RM} dist/Debug/GNU-Linux-x86/libopscpplinuxlib.a

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
