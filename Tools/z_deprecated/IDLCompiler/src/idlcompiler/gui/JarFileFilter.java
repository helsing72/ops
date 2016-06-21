/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package idlcompiler.gui;

import java.io.File;
import javax.swing.filechooser.FileFilter;
import util.FileHelper;

/**
 *
 * @author angr
 */
public class JarFileFilter extends FileFilter
{

    public boolean accept(File f)
    {
        String extension = FileHelper.getExtension(f);
        if(f.isDirectory())
            return true;
        else if(extension == null)
            return false;
        else if(extension.equals("jar"))
            return true;
        else
            return false;
    }

    public String getDescription()
    {
        return "Java JAR files (.jar)";
    }
}
