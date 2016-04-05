/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package opsc;

import configlib.ArchiverInOut;
import configlib.Serializable;
import java.io.IOException;

/**
 *
 * @author staxgr
 */
public class Property implements Serializable
{
    public String key = "";
    public String value = "";

    public Property(String k, String v)
    {
        key = k;
        value = v;
    }

    Property()
    {
        
    }

    public void serialize(ArchiverInOut archiver) throws IOException {
        key = archiver.inout("key", key);
        value = archiver.inout("value", value);
    }



}
