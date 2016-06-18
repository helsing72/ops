/*
 * IDLFile.java
 *
 * Created on den 31 juli 2007, 08:29
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package idlcompiler.files;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;

/**
 *
 * @author xhewe
 */
public class IDLFile implements TextFile
{
    
    
    private String text;
    private File file;

    private String relativPath;
    
    /** Creates a new instance of IDLFile */
    public IDLFile(File f, String basePath, String relativPath) 
    {
        
        this.setRelativPath(relativPath);
        file  = f;
        try
        {
            new File(basePath + "/" + relativPath).mkdirs();
            file.createNewFile();
        } 
        catch (IOException ex)
        {
            ex.printStackTrace();
        }
        try
        {
            
            FileInputStream fIn = new FileInputStream(file);
            byte[] b = null;
            try
            {
                b = new byte[fIn.available()];
            } 
            catch (IOException ex)
            {
                ex.printStackTrace();
            }
            try
            {
                fIn.read(b);
            } 
            catch (IOException ex)
            {
                ex.printStackTrace();
            }
            setText(new String(b));
            
        }
        catch (FileNotFoundException ex)
        {
            ex.printStackTrace();
        }
        
    }

    public String getText() 
    {
        return text;
    }
    
    public String getName()
    {
        return file.getName();
        //return file.getAbsolutePath();
    }
    
    public void setText(String text) 
    {
        this.text = text;
    }

    public File getFile() 
    {
        return file;
    }

    public String toString() 
    {
        return file.getName();
    }

    public void save()
    {
        FileOutputStream fOut = null;
        try 
        {
            fOut = new FileOutputStream(file);
        } 
        catch (FileNotFoundException ex) 
        {
            ex.printStackTrace();
        }
        try 
        {
            fOut.write(text.getBytes());
            fOut.close();
        } 
        catch (IOException ex) 
        {
            ex.printStackTrace();
        }
        
    }

    public String getRelativPath()
    {
        return relativPath;
    }

    public void setRelativPath(String relativPath)
    {
        this.relativPath = relativPath;
    }
    
    
    
    
}
