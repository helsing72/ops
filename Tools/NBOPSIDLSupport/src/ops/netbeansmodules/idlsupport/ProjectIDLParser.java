/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package ops.netbeansmodules.idlsupport;

import java.io.IOException;
import java.util.Vector;
import org.openide.util.Exceptions;
import org.openide.windows.IOProvider;
import org.openide.windows.InputOutput;
//import parsing.IDLClass;
//import parsing.ParseException;

/**
 *
 * @author angr
 */
public class ProjectIDLParser
{
//    Vector<IDLClass> idlClasses = new Vector<IDLClass>();
//    parsing.javaccparser.FileParser fileParser = new parsing.javaccparser.FileParser();
    //private InputOutput io;

    private int nrErrors = 0;
    private int nrWarnings = 0;

    public ProjectIDLParser()
    {
        //io = IOProvider.getDefault().getIO("OPS Build", false);
    }

    public void reset()
    {
     
            ///idlClasses.clear();
            nrErrors = 0;
            nrWarnings = 0;
            
    }
//    public void parse(String name, String fileText, InputOutput io)
//    {
//        try
//        {
//            IDLClass newClass = fileParser.parse(fileText);
//            if(newClass.getClassName().equals(name))
//            {
//                idlClasses.add(newClass);
//            }
//            else
//            {
//                io.getErr().println("In " + name + ", Error: File " + name + " does not contain a class called " + name + ".");
//                nrErrors++;
//            }
//        }
//        catch (ParseException ex)
//        {
//            io.getErr().println("In " + name + ", Error: " + ex.getMessage());
//            nrErrors++;
//        }
//    }

//    public Vector<IDLClass> getIdlClasses()
//    {
//        return idlClasses;
//    }


    public int getNrErrors()
    {
        return nrErrors;
    }

    public int getNrWarnings()
    {
        return nrWarnings;
    }


}
