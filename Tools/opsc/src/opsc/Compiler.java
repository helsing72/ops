/** -*-java-*- Copyright (C) 2014 Saab Dynamics AB
 ***************************************************************************
 *  @file   opsc/Compiler.java
 *  @author Mattias Helsing <mattias.helsing@saabgroup.com>
 *
 * Baseclass for my specialized compilers in this package
 *
 ***************************************************************************
 */

package opsc;

import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.Vector;
import parsing.IDLClass;
import parsing.IDLField;

/**
 *
 * @author helm
 */
public abstract class Compiler extends CompilerSupport
{

    public Compiler(String projectName) {
        super(projectName);
    }

    public abstract void compileDataClass(IDLClass idlClass) throws IOException;

    protected abstract void compileEnum(IDLClass idlClass) throws IOException;

    protected abstract void compilePublisher(IDLClass idlClass) throws IOException;

    protected abstract void compileSubscriber(IDLClass idlClass) throws IOException;

    protected abstract void compileTypeSupport(Vector<IDLClass> idlClasses, String projectName);

    //protected abstract String getClone(IDLClass idlClass);

    //protected abstract String getFillClone(IDLClass idlClass);

    protected abstract String getConstructorBody(IDLClass idlClass);

    //protected abstract CharSequence getConstructorHead(IDLClass idlClass);

    protected abstract String getDeclarations(IDLClass idlClass);

    protected abstract String getDeclareVector(IDLField field);

    protected abstract String languageType(String s);

    //protected abstract CharSequence getImports(IDLClass idlClass);

    //protected abstract CharSequence getDestructorBody(IDLClass idlClass);

    //protected abstract String getPackageCloser(String packageName);

    //protected abstract CharSequence getPackageDeclaration(String packageName);

    protected abstract String getSerialize(IDLClass idlClass);

    /**
     * @Override from IDLCompiler interface
     * */
    public void compileDataClasses(Vector<IDLClass> idlClasses, String projectDirectory)
    {
        //this.idlClasses = idlClasses;
        //this.projectDirectory = projectDirectory;
        for (IDLClass iDLClass : idlClasses)
        {
            compileIDLClass(iDLClass);
        }

        compileTypeSupport(_idlClasses, _projectName);
    }

    public void compileIDLClass(IDLClass idlClass)
    {
        try
        {
            if (idlClass.getType() == IDLClass.ENUM_TYPE)
            {
                System.out.println("Compile enum");
                compileEnum(idlClass);
            }
            else
            {
                compileDataClass(idlClass);
                compileSubscriber(idlClass);
                compilePublisher(idlClass);
            }

            _idlClasses.add(idlClass);

        } catch (IOException ioe) {
            System.out.println("Failed to generate code for " + idlClass.getClassName());
            System.out.println("  Generating failed with the following exception: " + ioe.getMessage());
        }
    }

    /**
     * Simple helper that create file on disc and writes text in it
     */
    public static void createAndWriteFile(String outFilePath, String outFileText) throws IOException
    {
        File outFile = new File(outFilePath);

        outFile.createNewFile();

        FileOutputStream fos = new FileOutputStream(outFile);

        fos.write(outFileText.getBytes());
        fos.close();
    }

}
