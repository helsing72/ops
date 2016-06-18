/*
 * ProjectListComponent.java
 *
 * Created on den 31 juli 2007, 11:00
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package idlcompiler.gui;

import idlcompiler.files.IDLFile;
import java.awt.Component;

/**
 *
 * @author xhewe
 */
public class ProjectListComponent extends Component {
    
    
    IDLFile file;
    
    /** Creates a new instance of ProjectListComponent */
    public ProjectListComponent(IDLFile f) 
    {
        file = f;
        
    }

    public String toString() 
    {
        return file.getName();
    }
    
    
    
    
    public String getName() 
    {
        return file.getName();
    }
    
}
