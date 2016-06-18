/*
 * Project.java
 *
 * Created on den 30 juli 2007, 21:33
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package idlcompiler.files;

import Exceptions.TopicConfigFileMissingException;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import java.util.Vector;

/**
 *
 * @author Anton Gravestam
 */
public class Project
{
    private Vector<IDLFile> IDLFiles;
    Vector<String> fileNames;
    String name;
    String runDirectory;

    private String relativeOutputPath = "";

    private TopicConfigFile topicConfigFile;
    private String topicConfigPackage;
    private boolean codeHighLighting;
    private boolean projectInvalid;
    private String invalidInfo = "";
    private boolean cppGeneration;
    private boolean csGeneration;
    private boolean javaGeneration;
    private Vector<String> jarDependencies = new Vector<String>();
    private boolean buildAndJar;

    public void enableBuildAndJar(boolean bool)
    {
        buildAndJar = bool;
    }

    public void setJarDeps(Vector<String> jarDepStringList)
    {
        setJarDependencies(jarDepStringList);
    }

    

    public void setRunDirectory(String runDirectory)
    {
        this.runDirectory = runDirectory;
    }

    public String getRunDirectory()
    {
        return runDirectory;
    } 

    public String getName()
    {
        return name;
    }

    public static Project Load(String n, String runDir)
    {
        return new Project(n, runDir);
    
    }
    
    public static Project Load(File file)
    {
        return new Project(file);
    
    }
    
    public void close()
    {
        getIDLFiles().removeAllElements();
   
    }
    
    public String getBaseDir()
    {
        String ret = name.substring(0,name.lastIndexOf("/") + 1) ;
        return ret;
    }
    
    
    
    /** Creates a new instance of Project */
    public Project(String name, String runDir)
    {
        
        //this.name = name.replaceAll("\\\\", "/");
        this.runDirectory = runDir;
        this.name = name;
        this.topicConfigPackage = name;
        setIDLFiles(new Vector<IDLFile>());
    }
    private Project(File projectFile)
    {
         setIDLFiles(new Vector<IDLFile>());
        
        try
        {
            runDirectory = projectFile.getParent() + "\\";
            FileInputStream fis = new FileInputStream(projectFile);
            
            byte[] b = new byte[fis.available()];
            fis.read(b);
            String fileString = new String(b);
            
            String[] fileArray = fileString.trim().split(",");
            
            
            this.name = fileArray[0].trim();
            this.setRelativeOutputPath(fileArray[1].trim());
            
            String[] topicConfigSettings = fileArray[2].split(":");            
            this.setTopicConfig(new TopicConfigFile(new File(projectFile.getParent() + "\\\\" + topicConfigSettings[0].trim())));
            try
            {
                this.setTopicConfigPackage(topicConfigSettings[1]);
            }
            catch(ArrayIndexOutOfBoundsException ex)
            {
                this.setTopicConfigPackage(getName());
            }
            
            //this.name = this.name.replaceAll("\\\\", "/");
            
            File f = null;
            
            for (int i = 3; i < fileArray.length; i++)
            {
                String s = projectFile.getParent() + "\\\\";
                f = new File(s + fileArray[i].trim());
                
                getIDLFiles().add(new IDLFile(f, runDirectory, fileArray[i].substring(0, fileArray[i].lastIndexOf("/") + 1).trim() ));
                
                
            }
            
            
            
            
        } 
        catch (FileNotFoundException ex)
        {
            ex.printStackTrace();
        } 
        catch (IOException ex)
        {
            ex.printStackTrace();
        }
        catch(ArrayIndexOutOfBoundsException ex)
        {
            ex.printStackTrace();
        }
        
        
        
    }
    public void save() throws TopicConfigFileMissingException
    {
        try
        {
            for (IDLFile idlfile : IDLFiles)
            {
                idlfile.save();
            }
            try
            {
                topicConfigFile.save();
            }
            catch (NullPointerException ex)
            {
                throw new TopicConfigFileMissingException();
            }
            
            ProjectFileGenerator.generateAndSave(this);
            
//            FileOutputStream fos = new FileOutputStream(new File(runDirectory + "\\" + name));
//            String outText = name + ",\n";
//            
//            outText += getRelativeOutputPath() + ",\n";
//            
//            outText += getTopicConfigFile().getFile().getName() + ":" + getTopicConfigPackage() + ",\n";
//            
//            for (IDLFile idlfile : IDLFiles)
//            {
//                outText += idlfile.getRelativPath() + idlfile.getName() + ",\n";
//            }
//                           
//            fos.write(outText.getBytes());
            
        } 
        catch (IOException ex)
        {
            ex.printStackTrace();
        }
        
        
    }
    public void addFile(IDLFile newFile)
    {
         for(IDLFile file: IDLFiles)
         {
            if (file == newFile )
                return;
         
         }       
         getIDLFiles().add(newFile);
    }
    public void removeFile(IDLFile f)
    {
        getIDLFiles().remove(f);
        
    }

    public void setRelativeOutputPath(String outputPath)
    {
        relativeOutputPath = outputPath;
    }

    public String getRelativeOutputPath()
    {
        return relativeOutputPath;
    }

    public void setTopicConfig(TopicConfigFile topicConfigFile)
    {
        this.topicConfigFile = topicConfigFile;
    }

    public TopicConfigFile getTopicConfigFile()
    {
        return topicConfigFile;
    }

    public Vector<IDLFile> getIDLFiles()
    {
        return IDLFiles;
    }

    public void setIDLFiles(Vector<IDLFile> IDLFiles)
    {
        this.IDLFiles = IDLFiles;
    }

    public String getTopicConfigPackage()
    {
        return topicConfigPackage;
    }

    public void setTopicConfigPackage(String topicConfigPackage)
    {
        this.topicConfigPackage = topicConfigPackage;
    }

    void addJarDependency(String value)
    {
        getJarDependencies().add(value);
    }
    void removeJarDependency(String value)
    {
        for(String s : getJarDependencies())
        {
            if(s.equals(value))
                getJarDependencies().remove(s);
        }
    }

    public void enableCppGeneration(boolean b)
    {
        cppGeneration = b;
    }

    public void enableCsGeneration(boolean b)
    {
        csGeneration = b;
    }
 
    public void enableJavaGeneration(boolean b)
    {
        javaGeneration = b;
    }

    public void enableCodeHighLighting(boolean bool)
    {
        codeHighLighting = bool;
    }
    boolean getCodeLightingEnabled()
    {
        return codeHighLighting;
    }

    void setInvalid(String reason)
    {
        invalidInfo += reason;
        projectInvalid = true;
    }

    public String getInvalidInfo()
    {
        return invalidInfo;
    }

    public boolean isCppGenerationEnabled()
    {
        return cppGeneration;
    }

    public boolean isCsGenerationEnabled()
    {
        return csGeneration;
    }

    public boolean isJavaGenerationEnabled()
    {
        return javaGeneration;
    }

    public boolean isBuildAndJarEnabled()
    {
        return buildAndJar;
    }

    public Vector<String> getJarDependencies()
    {
        return jarDependencies;
    }

    public void setJarDependencies(Vector<String> jarDependencies)
    {
        this.jarDependencies = jarDependencies;
    }
    
}
