
#ifndef ops_NoSuchTopicException_h
#define ops_NoSuchTopicException_h

#include <string.h>
#include <exception> 

namespace ops
{
	class NoSuchTopicException : public std::exception
	{
	public:
		NoSuchTopicException(std::string mess) : message(mess)
		{
		}
		const char* what() const noexcept { return message.c_str(); }

		~NoSuchTopicException() throw()
        {
        }
	private:
		std::string message;
	};
}
#endif
