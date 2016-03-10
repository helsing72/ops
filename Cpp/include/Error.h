#ifndef ops_Error_h
#define ops_Error_h

#include <string>

namespace ops
{
	///Interface for errors in OPS
	class Error
	{
	public:
		virtual int getErrorCode() = 0;
		virtual std::string getMessage() = 0;
		virtual ~Error(){}
	};


}
#endif
