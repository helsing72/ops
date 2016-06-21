/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package idlcompiler.gui;

import java.io.File;
import java.io.FilenameFilter;
import javax.swing.filechooser.FileFilter;
import util.FileHelper;

/**
 *
 * @author angr
 */
public class ProjectFileFilter extends FileFilter implements FilenameFilter
{

    public boolean accept(File f)
    {

        if (f.isDirectory())
        {
            return true;
        }

        String extension = FileHelper.getExtension(f);
        if (extension != null)
        {
            if (extension.equals("prj"))
            {
                return true;
            } 
            else
            {
                return false;
            }
        }

        return false;
    }

    public String getDescription()
    {
        return "OPS IDL Project files (.prj)";
    }

    public boolean accept(File dir, String name)
    {
        String extension = FileHelper.getExtension(new File(name));
        
        if(extension.equals("prj"))
            return true;
        else
            return false;
        
    }
}
