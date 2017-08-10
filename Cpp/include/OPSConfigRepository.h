/**
* 
* Copyright (C) 2016-2017 Lennart Andersson.
*
* This file is part of OPS (Open Publish Subscribe).
*
* OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
* it under the terms of the GNU Lesser General Public License as published by
* the Free Software Foundation, either version 3 of the License, or
* (at your option) any later version.

* OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
* GNU Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public License
* along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.
*/

#pragma once

#include <map>

#include "OPSTypeDefs.h"
#include "OPSConfig.h"

namespace ops
{
    class OPSConfigRepository 
    {
    public:
        // Access to the singleton object
        static OPSConfigRepository* Instance();

        // ======================================================
        // Add one or more domains from OPS configuration file "filename"
        // if 'domainID' == "", all domains will be added otherwise only the specified 'domainID'
        // Returns true if at least one domain was added
        bool Add( FileName_T filename, ObjectName_T domainID = "" );

        // Remove all domains from repository (Note does not clear the file-cache)
        void Clear();

        // ======================================================
        // Get a reference to the internal OPSConfig object
        // if 'domainID' != "", the domain 'domainID' must exist otherwise NULL is returned.
        OPSConfig* getConfig(ObjectName_T domainID = "" );

        bool domainExist(ObjectName_T domainID );

        // Just for Test
        void DebugTotalClear();

    private:
        OPSConfigRepository();

        // Our OPSConfig object containing references to all selectivly added domains
        OPSConfig* m_config;

        // File cache with all added config files and their domains
        std::map<FileName_T, OPSConfig*> m_configFiles;
    };

}
