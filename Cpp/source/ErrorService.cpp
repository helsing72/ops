
#include "OPSTypeDefs.h"
#include "ErrorService.h"

namespace ops
{
    void ErrorService::report(Error* error)
    {
        notifyNewEvent(error);
    }

    void ErrorService::report(ErrorMessage_T className, ErrorMessage_T methodName, ErrorMessage_T errorMessage)
    {
        UNUSED(className);
        UNUSED(methodName);
        UNUSED(errorMessage);
    }
}
