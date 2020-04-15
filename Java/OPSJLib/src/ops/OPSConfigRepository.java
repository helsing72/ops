/**
*
* Copyright (C) 2020 Lennart Andersson.
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
package ops;

import configlib.ArchiverInOut;
import configlib.XMLArchiverIn;
import configlib.XMLArchiverOut;
import configlib.exception.FormatException;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Vector;
import java.util.Hashtable;
import ops.OPSConfig;

/**
 *
 * @author lelle
 */
public class OPSConfigRepository
{
    private static class InternalConfig extends OPSConfig
    {
        public void add(Domain dom)
        {
            domains.add(dom);
        }
        public void clear()
        {
            domains.clear();
        }
        public int size()
        {
            return domains.size();
        }
    }

    //Static------------------------
    private static InternalConfig config = new InternalConfig();
    private static Hashtable<String, OPSConfig> files = new Hashtable<String, OPSConfig>();

    /**
     * Add one or more domains from OPS configuration file "filename"
     * if "domainID" == '', all domains will be added otherwise only the specified "domainID"
     * Returns true if at least one domain was added
     */
    public static synchronized boolean add(String filename)
    {
        return add(filename, "");
    }
    public static synchronized boolean add(String filename, String domainID)
    {
        if (domainID != "") {
            // Check if domain already exist
            if (domainExist(domainID)) {
                Logger.getLogger(OPSConfigRepository.class.getName()).log(Level.WARNING, ".Add(): domain already exist");
                return false;
            }
        }

        OPSConfig cfg = null;

        // Check if file already read
        if (files.containsKey(filename)) {
            cfg = files.get(filename);

        } else {
            // Need to read file
            try {
                cfg = OPSConfig.getConfig(new File(filename));
            } catch (IOException ex) {
                return false;
            } catch (FormatException ex) {
                return false;
            }
            if (cfg == null) { return false; }
            files.put(filename, cfg);
        }

        // Get all domains read from file
        Vector<Domain> domains = cfg.getDomains();
        boolean retVal = false;

        // Add the choosen one(s) to our list if not already there
        for (int i = 0; i < domains.size(); i++) {
            if ((domainID == "") || (domains.get(i).getDomainID() == domainID)) {
                if (domainExist(domains.get(i).getDomainID())) {
                    Logger.getLogger(OPSConfigRepository.class.getName()).log(Level.WARNING, ".Add(): domain already exist");
                } else {
                    // Add unique domains to our list
                    config.add(domains.get(i));
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
    public static synchronized void clear()
    {
        config.clear();
    }

    /**
     * Get a reference to the internal OPSConfig object
     * if "domainID" <> '', the domain "domainID" must exist otherwise NULL is returned.
     */
    public static synchronized OPSConfig getConfig()
    {
        return getConfig("");
    }
    public static synchronized OPSConfig getConfig(String domainID)
    {
        // If no domain have been added, we try to add the default file
        // This is for backward compatibility
        if (config.size() == 0) {
            if (!add("ops_config.xml")) return null;
        }

        if (domainID != "") {
            if (!domainExist(domainID)) return null;
        }

        return config;
    }

    public static synchronized boolean domainExist(String domainID)
    {
        Vector<Domain> domains = config.getDomains();
        for (int i = 0; i < domains.size(); i++) {
            if (domains.get(i).getDomainID() == domainID) {
                return true;
            }
        }
        return false;
    }

}
