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
import parsing.AbstractTemplateBasedIDLCompiler;
import parsing.IDLClass;
import parsing.IDLField;
import parsing.TopicInfo;

/**
 *
 * @author helm
 */
public abstract class Compiler extends AbstractTemplateBasedIDLCompiler
{
    /** Where to find templates. Default is in directory /templates in jar file IDLTemplates.jar */
    private String _templateDir = "templates";
    protected String _outputDir = ".";
    protected String _projectName = "";
    Vector<IDLClass> _idlClasses = new Vector<IDLClass>();
    protected Vector<String> _generatedFiles;

    /** A verbosity flag. Currently supports 0 or not 0 */
    protected int _verbose = 0;

    public void setVerbose(int value) {
      _verbose = value;
    }

    public Compiler(String projectName) {
        // set projectname
        _projectName = projectName;
        // tab is 4 spaces
        setTabString("    ");
        // endl is platform specific
        setEndlString(System.getProperty("line.separator"));
    }

    public abstract void compileDataClass(IDLClass idlClass) throws IOException;

    protected abstract void compileEnum(IDLClass idlClass) throws IOException;

    protected abstract void compilePublisher(IDLClass idlClass) throws IOException;

    protected abstract void compileSubscriber(IDLClass idlClass) throws IOException;

    protected abstract void compileTypeSupport(Vector<IDLClass> idlClasses, String projectName) throws IOException;

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



    public void setTemplateDir(String templatedir) {
        _templateDir = templatedir;
    }

    public void setOutputDir(String dir) {
        /** @todo Check and remove trailing slash */
        _outputDir = dir;
    }

    public void setProjectName(String name) {
        _projectName = name;
    }

    /**
     * @Override from IDLCompiler interface
     * */
    public void compileDataClasses(Vector<IDLClass> idlClasses, String projectDirectory)
    {
        try
        {
            //this.idlClasses = idlClasses;
            //this.projectDirectory = projectDirectory;
            for (IDLClass iDLClass : idlClasses)
            {
                compileIDLClass(iDLClass);
            }

            compileTypeSupport(_idlClasses, _projectName);

        } catch (IOException iOException) {
            System.out.println("Generating C++ failed with the following exception: " + iOException.getMessage());
        }
    }

    public void compileTypeSupport() {
        try {
            compileTypeSupport(_idlClasses, _projectName);
        } catch (IOException ioe) {
            System.out.println("Generating code failed with the following exception: " + ioe.getMessage());
        }
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
     * @Override empty so far from IDLCompiler interface
     * I don't know if this is really used somewhere. Please advise.
     * */
    public void compileTopicConfig(Vector<TopicInfo> topics, String name, String packageString, String projectDirectory)
    {
    }

    protected void setTemplateTextFromResource(java.io.InputStream stream) throws IOException
    {
        byte[] templateBytes = new byte[stream.available()];
        stream.read(templateBytes);
        setTemplateText(new String(templateBytes));
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

    protected java.io.InputStream findTemplateFile(String templateName) throws IOException {
        // search for a file called templateName
        File thefile = new File( templateName );

        //System.out.println("Looking for template: " + templateName);

        // if the path is absolute - use as is - no fallbacks
        if (thefile.isAbsolute()) {
            if (thefile.isFile()) {
                //System.out.println("found... " + thefile.getCanonicalPath());
                return new java.io.FileInputStream(thefile.getCanonicalPath());
            }
            throw new IOException("No such file " + templateName);
        }

        // try using _templateDir member
        thefile = new File( _templateDir + File.separator + templateName );
        if (thefile.isFile()) {
            //System.out.println("found... " + thefile.getCanonicalPath());
            return new java.io.FileInputStream(thefile.getCanonicalPath());
        }

        String resource = "/usr/share/ops/templates";
        thefile = new File( resource + File.separator + templateName );
        if (thefile.isFile()) {
            //System.out.println("found... " + thefile.getCanonicalPath());
            return new java.io.FileInputStream(thefile.getCanonicalPath());
        }

        resource = "../../share/ops/templates";
        thefile = new File( resource + File.separator + templateName );
        if (thefile.isFile()) {
            //System.out.println("found... " + thefile.getCanonicalPath());
            return new java.io.FileInputStream(thefile.getCanonicalPath());
        }

        resource = "/templates/" + templateName;
        if (this.getClass().getResource( resource ) != null) {
          //System.out.println("found... " + resource);
          return this.getClass().getResourceAsStream(resource);
        }

        resource = "/NBOPSIDLSupport/src/ops/netbeansmodules/idlsupport/templates/" + templateName;
        if (this.getClass().getResource( resource ) != null) {
          //System.out.println("found... " + resource);
          return this.getClass().getResourceAsStream(resource);
        }

        throw new IOException("No such template " + templateName);
    }

    /**
     * @override
     * */
    protected void saveOutputText(String templateText)
    {
        FileOutputStream fos = null;
        try
        {
            File outFile = new File(outputFileName);
            File outFilePath = new File(outputFileName.substring(0, outputFileName.lastIndexOf(File.separator)));

            outFilePath.mkdirs();
            outFile.createNewFile();


            fos = new FileOutputStream(outFile);
            fos.write(templateText.getBytes());
        } catch (IOException ex) {
            Logger.getLogger(AbstractTemplateBasedIDLCompiler.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            try {
                fos.close();
            } catch (IOException ex) {
                //Logger.getLogger(CppFactoryIDLCompiler.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

}
