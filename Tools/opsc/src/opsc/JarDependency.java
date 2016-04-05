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
 * @author angr
 */
public class JarDependency implements Serializable
{
    public String path = "";

    public JarDependency()
    {
    }

    
    public JarDependency(String relativePath)
    {
        path = relativePath;
    }

    public void serialize(ArchiverInOut archiver) throws IOException
    {
        path = archiver.inout("path", path);
    }

    @Override
    public String toString()
    {
        return path;
    }

}
