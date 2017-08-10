#ifndef ops_Error_h
#define ops_Error_h

#include "OPSTypeDefs.h"

namespace ops
{
	///Interface for errors in OPS
	class Error
	{
	public:
		virtual int getErrorCode() = 0;
		virtual ErrorMessage_T getMessage() = 0;
		virtual ~Error(){}
	};
}
#endif
