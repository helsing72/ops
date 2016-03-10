#ifndef ops_BasicError_h
#define ops_BasicError_h

#include <string>
#include "Error.h"

namespace ops
{
	///Basic implementaion of an error for OPS.
	class BasicError : public Error
	{
	public:
		static const int ERROR_CODE = 1;
		BasicError(std::string className, std::string method, std::string mess)
		{
			message = mess;
			this->className = className;
			this-> method = method;
		}
		virtual int getErrorCode()
		{
			return ERROR_CODE;
		}
		virtual std::string getMessage()
		{
			return className + "::" + method + "(): " + message;
		}
		virtual ~BasicError(){}
	private:
		std::string message;
		std::string className;
		std::string method;
	};


}
#endif
