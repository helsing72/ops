/*
 * TextFile.java
 *
 * Created on den 25 oktober 2007, 10:27
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package idlcompiler.files;

/**
 *
 * @author angr
 */
public interface TextFile
{
    public void setText(String text);
    public String getText();
    public void save();
    public String getName();
    
}
