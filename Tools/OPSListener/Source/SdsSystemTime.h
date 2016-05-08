/*

*/

#ifndef __SdsSystemTime_H__
#define __SdsSystemTime_H__

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

