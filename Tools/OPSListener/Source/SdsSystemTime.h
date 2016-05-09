/*

*/

#ifndef __SdsSystemTime_H__
#define __SdsSystemTime_H__

#ifdef _WIN32
  #if _MSC_VER < 1900
    // The int64_t type isn't defined before VS2015
    typedef __int64 int64_t;
  #endif
#else
  #include "inttypes.h"
#endif

namespace sds {

	// Returns time in SDS units (currently [ns])
	extern int64_t sdsSystemTime();
	extern int64_t msToSdsSystemTimeUnits(int64_t timeInMs);
	extern int64_t sdsSystemTimeUnitsToMs(int64_t timeInSdsUnits);
	extern void sdsSystemTimeInit();

	// Utility
	std::string sdsSystemTimeToLocalTime(int64_t time);
}

#endif // __SdsSystemTime_H__

