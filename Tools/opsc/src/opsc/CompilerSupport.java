/** -*-java-*- Copyright (C) 2014 Saab Dynamics AB
 ***************************************************************************
 *  @file   opsc/CompilerSupport.java
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
public abstract class CompilerSupport extends AbstractTemplateBasedIDLCompiler
{
    /** Where to find templates. Default is in directory /templates in jar file IDLTemplates.jar */
    private String _templateDir = "templates";
    protected String _outputDir = ".";
    protected String _projectName = "";
    Vector<IDLClass> _idlClasses = new Vector<IDLClass>();
    protected Vector<String> _generatedFiles;

    protected boolean _onlyGenTypeSupport = false;

    /** A verbosity flag. Currently supports 0 or not 0 */
    protected int _verbose = 0;

    public CompilerSupport(String projectName) {
        // set projectname
        _projectName = projectName;
        // tab is 4 spaces
        setTabString("    ");
        // endl is platform specific
        setEndlString(System.getProperty("line.separator"));
    }

    /**
     * @Override from IDLCompiler interface
     * */
    // This is currently not used for our compilers
    // Declare it here so specific compilers don't need to
    public void compileTopicConfig(Vector<TopicInfo> topics, String name, String packageString, String projectDirectory)
    {
    }

    public void setVerbose(int value) {
        _verbose = value;
    }

    public void setGenOnlyTypeSupport(boolean value)
    {
        _onlyGenTypeSupport = value;
    }

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

    protected void setTemplateTextFromResource(java.io.InputStream stream) throws IOException
    {
        byte[] templateBytes = new byte[stream.available()];
        stream.read(templateBytes);
        setTemplateText(new String(templateBytes));
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
            //System.out.println(">>>>: " + outputFileName);
            File outFile = new File(outputFileName);
            //System.out.println(">>>>: " + outFile.getAbsolutePath());
            File outFilePath = new File(outputFileName.substring(0, outputFileName.lastIndexOf(File.separator)));
            //System.out.println(">>>>: " + outFilePath.getAbsolutePath());

            outFilePath.mkdirs();
            outFile.createNewFile();

            fos = new FileOutputStream(outFile);
            fos.write(templateText.getBytes());
        } catch (IOException ex) {
            //System.out.println(">>>>: IOException");
            Logger.getLogger(CompilerSupport.class.getName()).log(Level.SEVERE, null, ex);
        } finally {
            try {
                fos.close();
            } catch (IOException ex) {
                //Logger.getLogger(CppFactoryIDLCompiler.class.getName()).log(Level.SEVERE, null, ex);
            }
        }
    }

}
