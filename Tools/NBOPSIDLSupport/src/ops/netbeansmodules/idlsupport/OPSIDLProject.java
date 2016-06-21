/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package ops.netbeansmodules.idlsupport;

import configlib.XMLArchiverIn;
import configlib.XMLArchiverOut;
import configlib.exception.FormatException;
import java.beans.PropertyChangeListener;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Properties;
import javax.swing.Icon;
import javax.swing.ImageIcon;
import ops.netbeansmodules.idlsupport.projectproperties.OPSProjectProperties;
import org.netbeans.api.project.Project;
import org.netbeans.api.project.ProjectInformation;
import org.netbeans.spi.project.ActionProvider;
import org.netbeans.spi.project.ProjectState;
import org.netbeans.spi.project.ui.LogicalViewProvider;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;
import org.openide.util.ImageUtilities;
import org.openide.util.Lookup;
import org.openide.util.lookup.Lookups;
import org.openide.windows.IOProvider;
import org.openide.windows.InputOutput;
import ops.netbeansmodules.util.FileHelper;
import ops.netbeansmodules.idlsupport.projectproperties.JarDependency;
/**
 *
 * @author angr
 */
public class OPSIDLProject implements Project
{

    public static final String SRC_DIR = "src";
    public static final String TOPIC_CONFIG_DIR = "topicconfig";
    public static final String PROJECT_DIR = "opsproject";
    public static final String PROJECT_PROPFILE = "project.properties";
    public static final String DEFAULT_PROPFILE = "opsidldefault.properties";
    private FileObject projectDir;
    private ProjectState state;
    LogicalViewProvider logicalView = new OPSIDLProjectLogicalView(this);
    ProjectIDLParser projectIDLParser = new ProjectIDLParser();
    ProjectIDLCompiler projectIDLCompiler = new ProjectIDLCompiler(this);

    ///LA To be able to access properties from the default option dialog
    public static OPSProjectProperties defaultProperties = createDefault();
    
    //
    private OPSProjectProperties properties = new OPSProjectProperties();

    public OPSIDLProject(FileObject projectDir, ProjectState state)
    {
        this.projectDir = projectDir;
        this.state = state;
    }

    private static OPSProjectProperties createDefault()
    {
        OPSProjectProperties prop = new OPSProjectProperties();
        try
        {
            File inFile = new File( System.getProperty("user.home") + "/" + DEFAULT_PROPFILE);
            XMLArchiverIn archiver = new XMLArchiverIn(new FileInputStream(inFile));
            archiver.add(OPSProjectProperties.getSerializableFactory());
            prop = (OPSProjectProperties)archiver.inout("properties", prop);
        }
        catch (IOException ex)
        {
        }
        catch (FormatException ex)
        {
        }
        return prop;
    }

    public static void saveDefault()
    {
        try
        {
            File outFile = new File( System.getProperty("user.home") + "/" + DEFAULT_PROPFILE);
            XMLArchiverOut archiver = new XMLArchiverOut(new FileOutputStream(outFile));
            archiver.setWriteTypes(false);
            archiver.inout("properties", defaultProperties);
        }
        catch (IOException ex)
        {
            Exceptions.printStackTrace(ex);
        }
    }

    private void saveProp(OPSProjectProperties prop)
    {
        try
        {
            File outFile = new File(getProjectDirectory().getPath() + "/" + PROJECT_DIR + "/" + PROJECT_PROPFILE);
            XMLArchiverOut archiver = new XMLArchiverOut(new FileOutputStream(outFile));
            archiver.setWriteTypes(false);
            archiver.inout("properties", prop);
        }
        catch (IOException ex)
        {
            Exceptions.printStackTrace(ex);
        }
    }
   
    public void save()
    {
        saveProp(getProperties());
    }

    private void makeRelativePaths()
    {
        // Try to make any absolute file paths in properties, realtive
        // to the project file
        String currentDirectory = getProjectDirectory().getPath();
        for (int index = 0; index < properties.javaBuildJarDependencies.size(); index++) {
            File tmp = new File(properties.javaBuildJarDependencies.elementAt(index).path);
            if (!tmp.isAbsolute()) continue;
            properties.javaBuildJarDependencies.setElementAt(
                new JarDependency(
                    FileHelper.unixSlashed(
                        FileHelper.getRelativePath(
                            new File(currentDirectory),
                            tmp))),
                index);
        }
        for (int index = 0; index < properties.csBuildDllDependencies.size(); index++) {
            File tmp = new File(properties.csBuildDllDependencies.elementAt(index).path);
            if (!tmp.isAbsolute()) continue;
            properties.csBuildDllDependencies.setElementAt(
                new JarDependency(
                    FileHelper.unixSlashed(
                        FileHelper.getRelativePath(
                            new File(currentDirectory),
                            tmp))),
                index);
        }
    }

    public void setDefaultProperties()
    {
        saveProp(defaultProperties);

        try
        {
            File inFile = new File(getProjectDirectory().getPath() + "/" + PROJECT_DIR + "/" + PROJECT_PROPFILE);
            XMLArchiverIn archiver = new XMLArchiverIn(new FileInputStream(inFile));
            archiver.add(OPSProjectProperties.getSerializableFactory());
            properties = (OPSProjectProperties) archiver.inout("properties", properties);

            // Try to make absolute file paths in default properties, realtive
            // to the project file
            makeRelativePaths();
        }
        catch (FormatException ex)
        {
        }
        catch (IOException ex)
        {
            Exceptions.printStackTrace(ex);
        }
    }

    FileObject getSourceFolder(boolean create)
    {
        FileObject result = projectDir.getFileObject(SRC_DIR);

        if (result == null && create)
        {
            try
            {
                result = projectDir.createFolder(SRC_DIR);
            } catch (IOException ioe)
            {
                Exceptions.printStackTrace(ioe);
            }
        }
        return result;
    }

    FileObject getTopicConfigFolder(boolean create)
    {
        FileObject result = projectDir.getFileObject(TOPIC_CONFIG_DIR);

        if (result == null && create)
        {
            try
            {
                result = projectDir.createFolder(TOPIC_CONFIG_DIR);
            } catch (IOException ioe)
            {
                Exceptions.printStackTrace(ioe);
            }
        }
        return result;
    }

    public OPSProjectProperties getProperties()
    {
        return properties;
    }

    public FileObject getProjectDirectory()
    {
        return projectDir;
    }

    private Lookup lkp;

    public Lookup getLookup()
    {
        if (lkp == null)
        {
            lkp = Lookups.fixed(new Object[]
                    {
                        this, //project spec requires a project be in its own lookup
                        state, //allow outside code to mark the project as needing saving
                        new ActionProviderImpl(this), //Provides standard actions like Build and Clean
                        loadProperties(), //The project properties
                        new Info(), //Project information implementation
                        logicalView, //Logical view of project implementation
                    });
        }
        return lkp;
    }

    void setProperties(OPSProjectProperties prop)
    {
        properties = prop;
    }

    private String getName()
    {
        return new Info().getDisplayName();
    }

    private void iterateFileObject(FileObject sourceFolder, InputOutput io)
    {
        for (FileObject fileObject : sourceFolder.getChildren())
        {
            if(fileObject.isFolder())
            {
                System.out.println("Folder " + fileObject.getName());
                iterateFileObject(fileObject, io);
            }
            else if(fileObject.getExt().toLowerCase().equals("idl"))
            {
//                try
//                {
//                    //int size = fileObject.getInputStream().available();
//
//                    //byte[] textBytes = new byte[size];
//                    //fileObject.getInputStream().read(textBytes);
//                    projectIDLParser.parse(fileObject.getName(), fileObject.asText()/*new String(textBytes)*/, io);
//                    //fileObject.getInputStream().close();
//                } catch (IOException ex)
//                {
//                    Exceptions.printStackTrace(ex);
//                }
            }
        }
    }
    
    private void CommandLineCompiler(InputOutput io)
    {
        String opscPath = System.getenv("OPS_OPSC_PATH");
        if (opscPath == null) {
            io.getOut().println("Info: Path to OPS command-line compiler \"opsc.bat\" can be set using env. symbol OPS_OPSC_PATH");
            opscPath = "opsc.bat";
        } else {
            opscPath += "\\opsc.bat";
            io.getOut().println("Info: OPS command-line compiler \"" + opscPath + "\" used (from env. symbol OPS_OPSC_PATH)");
        }

        try {
          ProcessBuilder pb = new ProcessBuilder(opscPath, "-P", getProjectDirectory().getPath());
          pb.redirectErrorStream(true);
          Process p = pb.start();
          InputStream inp = p.getInputStream();

          int c;
          while ((c = inp.read()) != -1) {
            io.getOut().write(c);
            io.getOut().flush();
          }
        }
        catch (IOException e) {
          io.getOut().println("Error: " + e.getMessage());
        }
    }
    
    public void build()
    {
        try {
            InputOutput io = IOProvider.getDefault().getIO("OPS Build - " + getName(), false);
            //io.getOut().reset();
            io.getOut().reset();
            io.select();
            io.getOut().println("Starting build of " + getName() + "...");
            
            // Call built-in parser/compiler
            //iterateFiles(io);
            
            // Call the command-line compiler opsc.bat/.sh
            CommandLineCompiler(io);

            if(projectIDLParser.getNrErrors() == 0)
            {
            }
            io.getOut().println("Build finished.");
        } catch (Exception ex)
        {
            Exceptions.printStackTrace(ex);
        }
    }

    private void iterateFiles(InputOutput io)
    {
        projectIDLParser.reset();
        iterateFileObject(getSourceFolder(false), io);
        if(projectIDLParser.getNrErrors() > 0)
        {
            //InputOutput io = IOProvider.getDefault().getIO("OPS Build - " + getName(), false);
            //io.select();
            io.getErr().println("Project parsing failed with " + projectIDLParser.getNrErrors() + " errors.");
        }
        else
        {
            //InputOutput io = IOProvider.getDefault().getIO("OPS Build - " + getName(), false);
            io.select();
            io.getOut().println("Parsing successful.");
//            projectIDLCompiler.compile(projectIDLParser.getIdlClasses(), io);
        }
    }

    private Properties loadProperties()
    {
        FileObject fob = projectDir.getFileObject(PROJECT_DIR + "/" + PROJECT_PROPFILE);
        Properties properties = new NotifyProperties(state);
        if (fob != null) {
            try {
                properties.load(fob.getInputStream());
                makeRelativePaths();
            } catch (Exception e)
            {
                Exceptions.printStackTrace(e);
            }
        }
        return properties;
    }

    private static class NotifyProperties extends Properties
    {

        private final ProjectState state;

        NotifyProperties(ProjectState state)
        {
            this.state = state;
        }

        @Override
        public Object put(Object key, Object val)
        {
            Object result = super.put(key, val);
            if (((result == null) != (val == null)) || (result != null &&
                    val != null && !val.equals(result)))
            {
                state.markModified();
            }
            return result;
        }
    }

    private final class ActionProviderImpl implements ActionProvider
    {
        private OPSIDLProject project;

        private ActionProviderImpl(OPSIDLProject project)
        {
            this.project = project;
        }



        public String[] getSupportedActions()
        {
            return new String[]
                    {
                        "build", "run", "CTL_GenerateCode"
                    };
        }

        public void invokeAction(String string, Lookup lookup) throws IllegalArgumentException
        {
//            try
//            {
                //do nothing
//                InputOutput io = IOProvider.getDefault().getIO("OPS Build", false);
//                io.getOut().reset();
//                io.select();
//                io.getOut().println("Starting build... of " + project.getName() );
//                io.getErr().println("Build is not yet implemented.");
//
//                project.iterateFiles();
                project.build();

//            } catch (IOException ex)
//            {
//                Exceptions.printStackTrace(ex);
//            }

        }

        /**
         *
         * @param string ska vara "hej"
         * @param lookup är en banan
         * @return
         * @throws java.lang.IllegalArgumentException
         */
        public boolean isActionEnabled(String string, Lookup lookup) throws IllegalArgumentException
        {
            return string.equals("build");
        }
    }

    /** Implementation of project system's ProjectInformation class */
    private final class Info implements ProjectInformation
    {

        public Icon getIcon()
        {
       

                //JOptionPane.showMessageDialog(null, new File(this.getClass().getResource("/ops/netbeansmodules/idlsupport/test.txt").getFile()));
             

            return new ImageIcon(ImageUtilities.loadImage(
                    "ops/netbeansmodules/idlsupport/opsprojecticon.GIF"));
        }

        /**
         *
         * @return
         */
        public String getName()
        {
            return getProjectDirectory().getName();
        }

        public String getDisplayName()
        {
            return getName();
        }

        public void addPropertyChangeListener(PropertyChangeListener pcl)
        {
            //do nothing, won't change
        }

        /**
         * 
         * @param pcl
         */
        public void removePropertyChangeListener(PropertyChangeListener pcl)
        {
            //do nothing, won't change
        }

        public Project getProject()
        {
            return OPSIDLProject.this;
        }
    }
}
