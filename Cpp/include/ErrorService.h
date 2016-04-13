#ifndef ops_ErrorService_h
#define ops_ErrorService_h

#include "Error.h"
#include "Notifier.h"

namespace ops 
{
	class ErrorService : public Notifier<Error*>
	{
	public:
		void report(Error* error);
		void report(std::string className, std::string methodName, std::string errorMessage);
		
	};

}

#endif
