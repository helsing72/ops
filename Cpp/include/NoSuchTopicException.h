
#ifndef ops_NoSuchTopicException_h
#define ops_NoSuchTopicException_h

#include <exception> 

#include "OPSTypeDefs.h"

namespace ops
{
	class NoSuchTopicException : public std::exception
	{
	public:
		NoSuchTopicException(ExceptionMessage_T mess) : message(mess)
		{
		}
		const char* what() const NOEXCEPT { return message.c_str(); }

		~NoSuchTopicException() throw()
        {
        }
	private:
		ExceptionMessage_T message;
	};
}
#endif
