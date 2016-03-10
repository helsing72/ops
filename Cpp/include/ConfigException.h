#ifndef ops_ConfigException_h
#define ops_ConfigException_h

#include <exception> 

namespace ops
{

    class ConfigException : std::exception
    {
    public:

        ConfigException(std::string mess)
        : message(mess)
        {

        }

        const char* what()
        {
            return message.c_str();
        }

        virtual ~ConfigException() throw ()
        {
        }
    private:
        std::string message;

    };

}
#endif

