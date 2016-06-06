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
	${OBJECTDIR}/main.o

# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=-lpthread -pthread
CXXFLAGS=-lpthread -pthread

# Fortran Compiler Flags
FFLAGS=

# Assembler Flags
ASFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=-L../../../C++/Netbeans\ C++/OPSCppLinuxLib/dist/Debug/GNU-Linux-x86 -L../../../../boost/boost_1_42_0/lib -lopscpplinuxlib ../../../../boost/boost_1_42_0/lib/libboost_date_time.a ../../../../boost/boost_1_42_0/lib/libboost_regex.a ../../../../boost/boost_1_42_0/lib/libboost_serialization.a ../../../../boost/boost_1_42_0/lib/libboost_signals.a ../../../../boost/boost_1_42_0/lib/libboost_system.a ../../../../boost/boost_1_42_0/lib/libboost_program_options.a ../../../../boost/boost_1_42_0/lib/libboost_prg_exec_monitor.a ../../../../boost/boost_1_42_0/lib/libboost_thread.a

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	${MAKE}  -f nbproject/Makefile-Debug.mk dist/Debug/GNU-Linux-x86/testsubscriber

dist/Debug/GNU-Linux-x86/testsubscriber: ../../../../boost/boost_1_42_0/lib/libboost_date_time.a

dist/Debug/GNU-Linux-x86/testsubscriber: ../../../../boost/boost_1_42_0/lib/libboost_regex.a

dist/Debug/GNU-Linux-x86/testsubscriber: ../../../../boost/boost_1_42_0/lib/libboost_serialization.a

dist/Debug/GNU-Linux-x86/testsubscriber: ../../../../boost/boost_1_42_0/lib/libboost_signals.a

dist/Debug/GNU-Linux-x86/testsubscriber: ../../../../boost/boost_1_42_0/lib/libboost_system.a

dist/Debug/GNU-Linux-x86/testsubscriber: ../../../../boost/boost_1_42_0/lib/libboost_program_options.a

dist/Debug/GNU-Linux-x86/testsubscriber: ../../../../boost/boost_1_42_0/lib/libboost_prg_exec_monitor.a

dist/Debug/GNU-Linux-x86/testsubscriber: ../../../../boost/boost_1_42_0/lib/libboost_thread.a

dist/Debug/GNU-Linux-x86/testsubscriber: ${OBJECTFILES}
	${MKDIR} -p dist/Debug/GNU-Linux-x86
	${LINK.cc} -o ${CND_DISTDIR}/${CND_CONF}/${CND_PLATFORM}/testsubscriber ${OBJECTFILES} ${LDLIBSOPTIONS} 

${OBJECTDIR}/main.o: nbproject/Makefile-${CND_CONF}.mk main.cpp 
	${MKDIR} -p ${OBJECTDIR}
	${RM} $@.d
	$(COMPILE.cc) -g -I../../../C++/include -I../../TestAll/Generated/C++ -I../../../../boost/boost_1_42_0/include -MMD -MP -MF $@.d -o ${OBJECTDIR}/main.o main.cpp

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf:
	${RM} -r build/Debug
	${RM} dist/Debug/GNU-Linux-x86/testsubscriber

# Subprojects
.clean-subprojects:

# Enable dependency checking
.dep.inc: .depcheck-impl

include .dep.inc
