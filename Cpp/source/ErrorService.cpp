#include "OPSTypeDefs.h"
#include "ErrorService.h"
#include "BasicError.h"
#include "BasicWarning.h"

namespace ops
{

    void ErrorService::report(Error* error)
    {
        notifyNewEvent(error);
    }

    void ErrorService::report(std::string className, std::string methodName, std::string errorMessage, Error::Severity_T severity)
    {
		if (severity == Error::warning) {
			BasicWarning error(className, methodName, errorMessage);
			notifyNewEvent(&error);
		} else {
			BasicError error(className, methodName, errorMessage);
			notifyNewEvent(&error);
		}
	}
}
