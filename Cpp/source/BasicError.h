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
		BasicError(std::string className, std::string method, std::string mess):
			_message(mess), _className(className), _method(method)
		{
		}
		virtual int getErrorCode()
		{
			return ERROR_CODE;
		}
		virtual std::string getMessage()
		{
			return _className + "::" + _method + "(): " + _message;
		}
		virtual ~BasicError(){}
	private:
		std::string _message;
		std::string _className;
		std::string _method;
	};
}
#endif
