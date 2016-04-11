#ifndef ops_NoSuchTopicException_h
#define ops_NoSuchTopicException_h

#include <exception> 

namespace ops
{
	class NoSuchTopicException : std::exception
	{
	public:
		NoSuchTopicException(std::string mess) 
			: message(mess)
		{

		}
		const char* what()
		{
			return message.c_str();
		}
                ~NoSuchTopicException() throw()
                {
                    
                }
	private:
		std::string message;

	};

}
#endif
