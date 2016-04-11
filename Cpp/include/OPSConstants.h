#ifndef ops_OPSConstants_h
#define ops_OPSConstants_h

#include <string>
#include <limits.h>

namespace ops
{
	class OPSConstants
	{
	public:
		const static int PACKET_MAX_SIZE = 60000;
		const static int MESSAGE_MAX_SIZE = 2400000;
		const static __int64 MAX_DEADLINE_TIMEOUT = LONG_MAX;
		//const static std::string DEFAULT_PARTICIPANT_ID("DEFAULT_PARTICIPANT");
	};
}

#endif
