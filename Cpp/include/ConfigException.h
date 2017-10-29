
#ifndef ops_ConfigException_h
#define ops_ConfigException_h

#include <exception> 

#include "OPSTypeDefs.h"

namespace ops
{
    class ConfigException : public std::exception
    {
    public:
        ConfigException(ExceptionMessage_T mess) : message(mess)
        {
        }

		const char* what() const NOEXCEPT { return message.c_str(); }

        virtual ~ConfigException() throw ()
        {
        }
    private:
		ExceptionMessage_T message;
    };
}
#endif
