/*

*/

#ifdef _WIN32
  #include "windows.h"
#endif
#include <string>
#include <time.h>

#include "OPSTypeDefs.h"
#include "SdsSystemTime.h"

namespace sds {

int64_t iBaseTime = 0;

// Must be called before sdsSystemTime() is used
void sdsSystemTimeInit()
{
#ifdef _WIN32
	SYSTEMTIME time70;
	FILETIME file70, fileNow;
	LARGE_INTEGER lTime70, lOffsetTime;
	DWORD dwTimeNow;

	time70.wDay = 1;
	time70.wDayOfWeek = 0;
	time70.wHour = 0;
	time70.wMilliseconds = 0;
	time70.wMinute = 0;
	time70.wMonth = 1;
	time70.wSecond = 0;
	time70.wYear = 1970;
	SystemTimeToFileTime(&time70, &file70);

	// Contains a 64-bit value representing the number of 100-nanosecond intervals since January 1, 1601 (UTC).
	lTime70.HighPart = file70.dwHighDateTime;
	lTime70.LowPart = file70.dwLowDateTime;

	// Synchronize
	GetSystemTimeAsFileTime(&fileNow);		// 100-nanosecond intervals
	dwTimeNow = timeGetTime();				// in [ms]

	lOffsetTime.HighPart = fileNow.dwHighDateTime;
	lOffsetTime.LowPart = fileNow.dwLowDateTime;

	// Calculate offset since 1 jan 1970 and make it 1 ns resolution
	iBaseTime = (lOffsetTime.QuadPart - lTime70.QuadPart) * 100;

	// Substract timeGetTime() offset
	iBaseTime -= msToSdsSystemTimeUnits((int64_t)timeGetTime());
#endif
}

int64_t msToSdsSystemTimeUnits(int64_t timeInMs)
{
	return timeInMs * 1000000;			// [ns]
}

int64_t sdsSystemTimeUnitsToMs(int64_t timeInSdsUnits)
{
	return timeInSdsUnits / 1000000;	// [ns]
}

int64_t sdsSystemTime()
{
#ifdef _WIN32
	return iBaseTime + msToSdsSystemTimeUnits((int64_t)timeGetTime());
#else
	struct timespec tt;
        //time_t   tv_sec;        /* seconds */
        //long     tv_nsec;       /* nanoseconds */
	clock_gettime(CLOCK_REALTIME, &tt);
	return ((int64_t)1000000000 * (int64_t)tt.tv_sec) + (int64_t)tt.tv_nsec;
#endif
}

std::string sdsSystemTimeToLocalTime(int64_t time)
{
#ifdef _WIN32
	int64_t frac;
	struct tm tmTime;

	frac = time;
	time /= msToSdsSystemTimeUnits(1000);
	frac -= (time * msToSdsSystemTimeUnits(1000));
	frac /= msToSdsSystemTimeUnits(1);

	// Convert to local time.
	_localtime64_s( &tmTime, &time );

	char tmp[256];
	sprintf_s(tmp, sizeof(tmp), "%.2d:%.2d:%.2d.%.3I64d", tmTime.tm_hour, tmTime.tm_min, tmTime.tm_sec, frac);

	return tmp;
#else
	UNUSED(time);
	return "TODO";
#endif
}

}
