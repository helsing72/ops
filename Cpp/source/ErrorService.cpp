#include "OPSTypeDefs.h"
#include "ErrorService.h"

namespace ops
{

    void ErrorService::report(Error* error)
    {
        notifyNewEvent(error);

    }

    void ErrorService::report(std::string className, std::string methodName, std::string errorMessage)
    {


    }
}
