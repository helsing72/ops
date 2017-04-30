
#ifndef ops_ConfigException_h
#define ops_ConfigException_h

#include <string.h>
#include <exception> 

namespace ops
{
    class ConfigException : public std::exception
    {
    public:
        ConfigException(std::string mess) : message(mess)
        {
        }

		const char* what() const noexcept { return message.c_str(); }

        virtual ~ConfigException() throw ()
        {
        }
    private:
        std::string message;
    };
}
#endif
