///////////////////////////////////////////////////////////
//  OPSConfigRepository.cs
//  Implementation of the Class OPSConfigRepository
//  Created on:      2020-04-13
//  Author:          Lennart Andersson
///////////////////////////////////////////////////////////

using System.Runtime.CompilerServices;  // Needed for the "MethodImpl" synchronization attribute
using System.Collections.Generic;       // Needed for the "List"
using System.IO;
using System.Text;

namespace Ops
{
    public class OPSConfigRepository
    {
        private class InternalConfig : OPSConfig
        {
            public void Add(Domain dom)
            {
                domains.Add(dom);
            }
            public void Clear()
            {
                domains.Clear();
            }
            public int Count()
            {
                return domains.Count;
            }
        }

        private static InternalConfig config = new InternalConfig();
        private static Dictionary<string, OPSConfig> files = new Dictionary<string, OPSConfig>();

        /**
         * Add one or more domains from OPS configuration file "filename"
         * if "domainID" == '', all domains will be added otherwise only the specified "domainID"
         * Returns true if at least one domain was added
         */
        [MethodImpl(MethodImplOptions.Synchronized)]
        public static bool Add(string filename, string domainID = "")
        {
            if (domainID != "") {
                // Check if domain already exist
                if (DomainExist(domainID)) {
                    Logger.ExceptionLogger.LogMessage("OPSConfigRepository::Add(), domain already exist");
                    return false;
                }
            }

            OPSConfig cfg = null;

            // Check if file already read
            if (files.ContainsKey(filename)) {
                cfg = files[filename];

            } else {
                // Need to read file
                cfg = OPSConfig.GetConfig(filename);
                if (cfg == null) { return false; }
                files[filename] = cfg;
            }

            // Get all domains read from file 
            List<Domain> domains = cfg.getDomains();
            bool retVal = false;

            // Add the choosen one(s) to our list if not already there
            for (int i = 0; i < domains.Count; i++) {
                if ((domainID == "") || (domains[i].GetDomainID() == domainID)) {
                    if (DomainExist(domains[i].GetDomainID())) {
                        Logger.ExceptionLogger.LogMessage("OPSConfigRepository::Add(), domain already exist");
                    } else {
                        // Add unique domains to our list
                        config.Add(domains[i]);
                        retVal = true;
		        	}
                }
        	}

            return retVal;
        }

        /**
         * Remove all domain references from the repository (Note does not clear the file-cache)
         * Note: Calling this while Participant, Publisher or Subscriber instances exist
         * may have unwanted side effects
         */
        [MethodImpl(MethodImplOptions.Synchronized)]
        public static void Clear()
        {
            config.Clear();
        }

        /**
         * Get a reference to the internal OPSConfig object
         * if "domainID" <> '', the domain "domainID" must exist otherwise NULL is returned.
         */
        [MethodImpl(MethodImplOptions.Synchronized)]
        public static OPSConfig GetConfig(string domainID = "")
        {
            // If no domain have been added, we try to add the default file
            // This is for backward compatibility
            if (config.Count() == 0) {
                if (!Add("ops_config.xml")) return null;
            }

            if (domainID != "") {
                if (!DomainExist(domainID)) return null;
            }

            return config;
        }

        [MethodImpl(MethodImplOptions.Synchronized)]
        public static bool DomainExist(string domainID)
        {
            List<Domain> domains = config.getDomains();
            for (int i = 0; i < domains.Count; i++) {
                if (domains[i].GetDomainID() == domainID) {
                    return true;
                }
            }
            return false;
        }

    }
}
