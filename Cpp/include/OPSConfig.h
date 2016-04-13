#ifndef ops_OPSConfig_h
#define ops_OPSConfig_h

#include <vector>
#include "Topic.h"
#include "Domain.h"

namespace ops
{
	class OPSConfig : public OPSObject
	{
	public:
		static OPSConfig* getConfig();
		static OPSConfig* getConfig(std::string configFile);

		virtual Domain* getDomain(std::string domainID)
		{
			for(unsigned int i = 0; i < domains.size(); i++)
			{
				if(domains[i]->getDomainID() == domainID)
				{
					return domains[i];
				}
			}
			return NULL;

		}

		void serialize(ArchiverInOut* archiver)
		{

			OPSObject::serialize(archiver);
		
			archiver->inout<Domain>(std::string("domains"), domains);
		}

		std::vector<Domain*> getDomains()
		{
			return domains;
		}

	private:
		std::vector<Domain* > domains;

	};
}

#endif
