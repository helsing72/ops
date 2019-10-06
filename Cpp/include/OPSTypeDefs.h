/**
 *
 * Copyright (C) 2006-2010 Anton Gravestam.
 * Copyright (C) 2018-2019 Lennart Andersson.
*
 * This file is part of OPS (Open Publish Subscribe).
 *
 * OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.

 * OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
 */

 /*
 * File:   OPSTypeDefs.h
 * Author: gravanto
 *
 * Created on February 18, 2010, 9:52 PM
 */

#pragma once

#include <string>
#include <sstream>

// -----------------------------------------------------------------------------
// Some OPS configurations

// We require a c++11 compiler.
#if __cplusplus >= 201103L		// Value according to standard for full C++11 conformity
	#define OPS_C11_DETECTED
#elif defined(_MSC_VER) && (_MSC_VER >= 1900)
	// VS2015 still defines _cplusplus to 199711L but supports the features we need.
	// VS2013 an earlier also defines _cplusplus to 199711L but does not support the features.
	#define OPS_C11_DETECTED
#endif
#ifndef OPS_C11_DETECTED
#error C++11 Compiler required
#endif

#if __cplusplus >= 201402L		// Value according to standard for full C++14 conformity
	#define OPS_C14_DETECTED
#elif defined(_MSC_VER) && (_MSC_VER >= 1916)
	#if _MSVC_LANG >= 201402L
		#define OPS_C14_DETECTED
	#endif
#endif

#if __cplusplus >= 201703L		// Value according to standard for full C++17 conformity
	#define OPS_C17_DETECTED
#elif defined(_MSC_VER) && (_MSC_VER >= 1916)
	#if _MSVC_LANG >= 201703L
		#define OPS_C17_DETECTED
	#endif
#endif

#define NOEXCEPT noexcept


//#define OPS_REMOVE_ASSERT			// Define to skip assert() calls in OPS code.
									// (can also be done by defining NDEBUG)

//#define OPSSLIM_NORESERVE			// Removes Reservable from OPSMessage

//#define REPLACE_TRANSPORT_LAYER	// Removes IOService.cpp, Sender.cpp, Receiver.cpp, DeadlineTimer.cpp,
									// NetworkSupport.cpp and TimeHelper.cpp
									// from library so you can use your own implementations.

//#define REPLACE_OPS_CONFIG		// Removes the OPSConfig file reader from library so you can implement 
									// your own for targets without a filesystem.

//#define REPLACE_NETWORK_ALLOC		// Removes the ops::DataSegmentPool::Allocate/Deallocate from library so you
									// can use our own implementation.

#define OPS_ENABLE_DEBUG_HANDLER	// Adds some debug functionality

// -----------------------------------------------------------------------------
// Configure the fixed length string implementation to NOT have std::string support members.
#define FIXED_NO_STD_STRING
#include "fixed_string.h"
#include "fixed_string_support.h"

namespace ops {

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

	typedef strings::fixed_string<FIXED_OBJECT_NAME_SIZE>     ObjectName_T;
	typedef strings::fixed_string<FIXED_FILENAME_SIZE>        FileName_T;
	typedef strings::fixed_string<FIXED_MESSAGE_KEY_SIZE>     ObjectKey_T;
	typedef strings::fixed_string<FIXED_TYPE_ID_SIZE>         TypeId_T;
	typedef strings::fixed_string<FIXED_CHANNEL_ID_SIZE>      ChannelId_T;
	// OPS internal
	typedef strings::fixed_string<FIXED_PART_KEY_SIZE>        ParticipantKey_T;
	typedef strings::fixed_string<FIXED_ADDRESS_SIZE>         Address_T;
	typedef strings::fixed_string<FIXED_TRANSPORT_SIZE>       Transport_T;
	typedef strings::fixed_string<FIXED_INTERNAL_STRING_SIZE> InternalString_T;
	typedef strings::fixed_string<FIXED_EXCEPTION_MSG_SIZE>   ExceptionMessage_T;
	typedef strings::fixed_string<FIXED_INTERNAL_KEY_SIZE>    InternalKey_T;
	typedef strings::fixed_string<FIXED_ERROR_MSG_SIZE>       ErrorMessage_T;
	typedef const char*                                       InoutName_T;

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

// Define this to add counting of create/delete of OPSObject()
// Also adds a debug function for reading the current number of living OPSObjects
//#define DEBUG_OPSOBJECT_COUNTER

// -----------------------------------------------------------------------------
// Macros used for trace of some functionality used during development 
//#define OPS_TRACE(msg) { std::cout << msg << std::flush; }
#define OPS_TRACE(msg) 

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

// Defines for integer types
#ifdef _WIN32

#else 

// Linux
#include "inttypes.h"

typedef int64_t __int64;	// Deprecated, just for backward compatibility (used by some users) 
typedef int16_t __int16;	// - " -

#endif

// ---------------------------------------------
// Construct a compilation signature to be able to detect mismatched headers and compiled libraries.
// The compile signature is compiled into libraries and are checked at runtime against the define.

#ifdef FIXED_NO_STD_STRING
	#define COMPILESIGNATURE_FIXED ""
#else
	#define COMPILESIGNATURE_FIXED "STD"
#endif

#define COMPILESIGNATURE_CXX "C11"

#ifdef OPSSLIM_NORESERVE
	#define COMPILESIGNATURE_SLIM "SLIM"
#else
	#define COMPILESIGNATURE_SLIM ""
#endif

#ifdef OPS_ENABLE_DEBUG_HANDLER
	#define COMPILESIGNATURE_DBGHND "DBGHND"
#else
	#define COMPILESIGNATURE_DBGHND ""
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

#define OPS_COMPILESIGNATURE (COMPILESIGNATURE_FIXED COMPILESIGNATURE_CXX COMPILESIGNATURE_SLIM COMPILESIGNATURE_DBGHND COMPILESIGNATURE_STRINGS COMPILESIGNATURE_CTR)

