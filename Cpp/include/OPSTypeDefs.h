/*
 * File:   OPSTypeDefs.h
 * Author: gravanto
 *
 * Created on February 18, 2010, 9:52 PM
 */

#ifndef _OPSTYPEDEFS_H
#define _OPSTYPEDEFS_H

#include <string>

// Configure the fixed length string to NOT have std::string support members.
#define FIXED_NO_STD_STRING
#include "fixed_string.h"
#include "fixed_string_support.h"

#include <sstream>

namespace ops {

// -----------------------------------------------------------------------------
// Some OPS configurations
#define USE_C11             // Enables use of C++11 std::mutex, std::thread, std::condition_variable
							// instead of boost or WIN32/Linux specific calls

#ifdef OVERRIDE_DEFAULT_NO_C11	// Give possibility to override default settings for backward compatibility
#undef USE_C11					// This will use boost and WIN32/Linux specific calls instead of C++11
#endif


//#define OPSSLIM_NORESERVE			// Removes Reservable from OPSMessage

//#define REPLACE_TRANSPORT_LAYER	// Removes IOService.cpp, Sender.cpp, Receiver.cpp and DeadlineTimer.cpp
									// from library so you can use your own implementations.

//#define REPLACE_OPS_CONFIG		// Removes the OPSConfig file reader from library so you can implement
									// your own for targets without a filesystem.

// -----------------------------------------------------------------------------
// Defines for String handling in OPS

//#define USE_FIXED_LENGTH_STRINGS

#ifdef USE_FIXED_LENGTH_STRINGS
	// Define lengths if not already done via build system

	#ifndef FIXED_OBJECT_NAME_SIZE
		// Max name length for: DomainId, ParticipantId, TopicName, PublisherName, SubscriberName, ...
		// If you use ops::utilities::nnn() the length need to be able to handle Domain::TopicName
		#define FIXED_OBJECT_NAME_SIZE 50
	#endif
	#ifndef FIXED_MESSAGE_KEY_SIZE
		// Max length of key set by user on publisher/message and subscriber filter
		#define FIXED_MESSAGE_KEY_SIZE 60
	#endif
	#ifndef FIXED_TYPE_ID_SIZE
		// Max TypeString length and depends on type names and inheritance depth
		#define FIXED_TYPE_ID_SIZE 256
	#endif
	#ifndef FIXED_CHANNEL_ID_SIZE
		#define FIXED_CHANNEL_ID_SIZE 20
	#endif
	#ifndef FIXED_FILENAME_SIZE
		#define FIXED_FILENAME_SIZE 1024
	#endif


	// Rest of lengths are internal or defined in relation to the above lengths
	// Need to be able to handle Domain::ParticipantId
	#define FIXED_PART_KEY_SIZE (FIXED_OBJECT_NAME_SIZE + 2 + FIXED_OBJECT_NAME_SIZE)
	// xxx.xxx.xxx.xxx/xxx.xxx.xxx.xxx
	#define FIXED_ADDRESS_SIZE 32
	#define FIXED_TRANSPORT_SIZE 10
	#define FIXED_INTERNAL_STRING_SIZE 64
	#define FIXED_EXCEPTION_MSG_SIZE 256
	#define FIXED_ERROR_MSG_SIZE 256
	// transport::xxx.xxx.xxx.xxx::port
	#define FIXED_INTERNAL_KEY_SIZE (FIXED_TRANSPORT_SIZE + 2 + FIXED_ADDRESS_SIZE + 2 + 5)

	typedef fixed_string<FIXED_OBJECT_NAME_SIZE>     ObjectName_T;
	typedef fixed_string<FIXED_FILENAME_SIZE>        FileName_T;
	typedef fixed_string<FIXED_MESSAGE_KEY_SIZE>     ObjectKey_T;
	typedef fixed_string<FIXED_TYPE_ID_SIZE>         TypeId_T;
	typedef fixed_string<FIXED_CHANNEL_ID_SIZE>      ChannelId_T;
	// OPS internal
	typedef fixed_string<FIXED_PART_KEY_SIZE>        ParticipantKey_T;
	typedef fixed_string<FIXED_ADDRESS_SIZE>         Address_T;
	typedef fixed_string<FIXED_TRANSPORT_SIZE>       Transport_T;
	typedef fixed_string<FIXED_INTERNAL_STRING_SIZE> InternalString_T;
	typedef fixed_string<FIXED_EXCEPTION_MSG_SIZE>   ExceptionMessage_T;
	typedef fixed_string<FIXED_INTERNAL_KEY_SIZE>    InternalKey_T;
	typedef fixed_string<FIXED_ERROR_MSG_SIZE>       ErrorMessage_T;
	typedef const char*                              InoutName_T;

#else
	typedef std::string ObjectName_T;
	typedef std::string FileName_T;
	typedef std::string ObjectKey_T;
	typedef std::string TypeId_T;
	typedef std::string ChannelId_T;
	// OPS internal
	typedef std::string ParticipantKey_T;
	typedef std::string Address_T;
	typedef std::string Transport_T;
	typedef std::string InternalString_T;
	typedef std::string ExceptionMessage_T;
	typedef std::string InternalKey_T;
	typedef std::string ErrorMessage_T;
	typedef const std::string& InoutName_T;
#endif

// -----------------------------------------------------------------------------
// OPS uses Little Endian data serialization to improve the performance since
// thats the native packing for x86 and it also works on Arm.
// If OPS is compiled for a Big Endian machine (and it need to communicate with a
// little endian machine via OPS) you need to uncomment the following define to
// keep the binary compatibility.
//
//#define ON_BIG_ENDIAN_MACHINE

// Define this to add counting of create/delete of OPSObject() (only if also USE_C11 is defined)
// Also adds a debug function for reading the current number of living OPSObjects
//#define DEBUG_OPSOBJECT_COUNTER

// -----------------------------------------------------------------------------
// Macro used to remove compiler warnings about non used variables/parameters
#define UNUSED(expr) (void)(expr);

// Helper for converting a number to a string
template <typename T>
InternalString_T NumberToString(T Number)
{
	std::ostringstream ss;
	ss << Number << std::ends;
	return ss.str().c_str();
}

} // namespace

#ifndef _WIN32

#define OPS_LINUX

#endif

#ifdef OPS_LINUX

#include "inttypes.h"

typedef  int64_t __int64;
typedef  int16_t __int16;

#endif

// ---------------------------------------------
// Construct a compilation signature to be able to detect mismatched headers and compiled libraries.
// The compile signature is compiled into libraries and are checked at runtime against the define.

#ifdef FIXED_NO_STD_STRING
	#define COMPILESIGNATURE_FIXED ""
#else
	#define COMPILESIGNATURE_FIXED "STD"
#endif

#ifdef USE_C11
	#define COMPILESIGNATURE_CXX "C11"
#else
	#define COMPILESIGNATURE_CXX ""
#endif

#ifdef OPSSLIM_NORESERVE
	#define COMPILESIGNATURE_SLIM "SLIM"
#else
	#define COMPILESIGNATURE_SLIM ""
#endif

#ifdef USE_FIXED_LENGTH_STRINGS
	#define stringer(x) stringerx(x)
	#define stringerx(a) #a
	#define COMPILESIGNATURE_STRINGS \
		stringer(FIXED_OBJECT_NAME_SIZE) " " \
		stringer(FIXED_MESSAGE_KEY_SIZE) " " \
		stringer(FIXED_TYPE_ID_SIZE) " " \
		stringer(FIXED_CHANNEL_ID_SIZE) " " \
		stringer(FIXED_FILENAME_SIZE)
#else
	#define COMPILESIGNATURE_STRINGS "STD"
#endif

#ifdef DEBUG_OPSOBJECT_COUNTER
	#define COMPILESIGNATURE_CTR "CTR"
#else
	#define COMPILESIGNATURE_CTR ""
#endif

#define OPS_COMPILESIGNATURE (COMPILESIGNATURE_FIXED COMPILESIGNATURE_CXX COMPILESIGNATURE_SLIM COMPILESIGNATURE_STRINGS COMPILESIGNATURE_CTR)

#endif	/* _OPSTYPEDEFS_H */
