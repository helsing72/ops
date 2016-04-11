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
CC=gcc.exe
CCC=g++.exe
CXX=g++.exe
FC=

# Include project Makefile
include Makefile

# Object Directory
OBJECTDIR=build/Release/Cygwin-Windows

# Object Files
OBJECTFILES= \
	${OBJECTDIR}/source/boost/error_code.o \
	${OBJECTDIR}/source/OPSObject.o \
	${OBJECTDIR}/source/ByteBuffer.o \
	${OBJECTDIR}/source/UDPSender.o

# C Compiler Flags
CFLAGS=

# CC Compiler Flags
CCFLAGS=
CXXFLAGS=

# Fortran Compiler Flags
FFLAGS=

# Link Libraries and Options
LDLIBSOPTIONS=

# Build Targets
.build-conf: ${BUILD_SUBPROJECTS}
	${MAKE}  -f nbproject/Makefile-Release.mk dist/Release/Cygwin-Windows/libopscrossplatformcpplib.a

dist/Release/Cygwin-Windows/libopscrossplatformcpplib.a: ${OBJECTFILES}
	${MKDIR} -p dist/Release/Cygwin-Windows
	${RM} dist/Release/Cygwin-Windows/libopscrossplatformcpplib.a
	${AR} rv dist/Release/Cygwin-Windows/libopscrossplatformcpplib.a ${OBJECTFILES} 
	$(RANLIB) dist/Release/Cygwin-Windows/libopscrossplatformcpplib.a

${OBJECTDIR}/source/boost/error_code.o: source/boost/error_code.cpp 
	${MKDIR} -p ${OBJECTDIR}/source/boost
	${RM} $@.d
	$(COMPILE.cc) -O2 -MMD -MP -MF $@.d -o ${OBJECTDIR}/source/boost/error_code.o source/boost/error_code.cpp

${OBJECTDIR}/source/OPSObject.o: source/OPSObject.cpp 
	${MKDIR} -p ${OBJECTDIR}/source
	${RM} $@.d
	$(COMPILE.cc) -O2 -MMD -MP -MF $@.d -o ${OBJECTDIR}/source/OPSObject.o source/OPSObject.cpp

${OBJECTDIR}/source/ByteBuffer.o: source/ByteBuffer.cpp 
	${MKDIR} -p ${OBJECTDIR}/source
	${RM} $@.d
	$(COMPILE.cc) -O2 -MMD -MP -MF $@.d -o ${OBJECTDIR}/source/ByteBuffer.o source/ByteBuffer.cpp

${OBJECTDIR}/source/UDPSender.o: source/UDPSender.cpp 
	${MKDIR} -p ${OBJECTDIR}/source
	${RM} $@.d
	$(COMPILE.cc) -O2 -MMD -MP -MF $@.d -o ${OBJECTDIR}/source/UDPSender.o source/UDPSender.cpp

# Subprojects
.build-subprojects:

# Clean Targets
.clean-conf:
	${RM} -r build/Release
	${RM} dist/Release/Cygwin-Windows/libopscrossplatformcpplib.a

# Subprojects
.clean-subprojects:

# Enable dependency checking
include .dep.inc
