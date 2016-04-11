#ifndef ops_ErrorWriter_h
#define ops_ErrorWriter_h

#include <string>
#include <ostream>
#include "Listener.h"
#include "TimeHelper.h"

namespace ops
{
	///Utility class for writing Error to an ostream
	class ErrorWriter : public Listener<Error*>
	{
	public:
		ErrorWriter(std::ostream& os) : oStream(os)
		{
			
		}
		void onNewEvent(Notifier<Error*>* notifier, Error* error)
		{
			oStream << "@" << TimeHelper::getTimeToString() << " - Error code: " << error->getErrorCode() << ". Message: " << error->getMessage() << "." << std::endl;
		}
	private:
		std::ostream& oStream;
	};


}
#endif