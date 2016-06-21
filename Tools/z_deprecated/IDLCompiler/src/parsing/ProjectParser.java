/*
 * ProjectParser.java
 *
 * Created on den 12 november 2007, 08:08
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package parsing;

import idlcompiler.files.IDLFile;
import idlcompiler.files.Project;
import java.util.Vector;

/**
 *
 * @author angr
 */
public class ProjectParser
{
    Project project;
    
    private Vector<IDLClass> idlClasses = new Vector<IDLClass>();
    /** Creates a new instance of ProjectParser */
    public ProjectParser()
    {
        
    }
    public void parse(Project project) throws ParseException
    {

        parsing.javaccparser.FileParser fileParser = new parsing.javaccparser.FileParser();
        
        for (IDLFile file : project.getIDLFiles())
        {
            try
            {
                IDLClass testClass  = fileParser.parse(file.getText());
                
                testClass = (new TypeSorter()).resort(testClass);
                
                idlClasses.add(testClass);
                
            } 
            catch (ParseException ex)
            {
                throw new ParseException("In file " + file.getName() + ": " + ex.getMessage() + "");
            }
            
        }
    }

    public Vector<IDLClass> getIdlClasses()
    {
        return idlClasses;
    }

    public void setIdlClasses(Vector<IDLClass> idlClasses)
    {
        this.idlClasses = idlClasses;
    }
    
}
