/*
 * CSTopicConfigCreator.java
 *
 * Created on den 26 oktober 2007, 09:59
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package idlcompiler.topicconfigcreators;

import idlcompiler.files.Project;
import java.io.File;
import java.io.IOException;
import ops.StaticManager;
import ops.Topic;
import util.FileHelper;

/**
 *
 * @author angr
 */
public class CSTopicConfigCreator
{
    private String outfilePath;
    /** Creates a new instance of CSTopicConfigCreator */
    public CSTopicConfigCreator(Project project, String packageString)
    {
        String configFileString = "file:///" +  project.getRunDirectory().replace("\\", "/") + "/" + project.getTopicConfigFile().getFile().getName();
        //JOptionPane.showMessageDialog(null, configFileString);
        StaticManager manager = StaticManager.initializeManager(configFileString);
        
        
        String output = "";
        output += "namespace " + packageString +"\n{\n";
        String className = project.getName().replace(".prj", "") + "TopicConfig";
        output += "public class " + className + "\n";
        output += "{\n";
        output += "\tprivate string domainAddress = null;\n";
        output += "\tpublic " +  className +"(string domainAddress)\n";
        output += "\t{\n";
            output += "\t\tthis.domainAddress = domainAddress;\n";
        output += "\t}\n";
        
        for (Topic t : manager.getTopics())
        {
            output += "\tpublic OPS.Topic get" + t.getName() + "()\n";
            output += "\t{\n";
                output += "\t\treturn new OPS.Topic(\"" + t.getName() + "\", " + t.getPort() + ", \"" + t.getTypeID() + "\", this.domainAddress);\n";
            output += "\t}\n";
            
        }
        //Close class
        output += "}\n";
        //Close namespace
        output += "}\n";
        try
        {
            String outdirPath = FileHelper.unixSlashed(project.getRunDirectory()) + "/" + project.getRelativeOutputPath() + "/" + "C#/" + packageString.replace(".", "/") + "/";
            new File(outdirPath).mkdirs();
            outfilePath =  outdirPath + FileHelper.cropExtension(project.getName()) +  "TopicConfig.cs";
            
            idlcompiler.compilers.Compiler.createAndWriteFile(outfilePath, output);
            
            outfilePath = "\"" + outfilePath + "\"";
            
//            String execString = "javac -cp OPSJLib/OPSJLib.jar \"" + getOutfilePath() + "\"";
//            Runtime.getRuntime().exec(execString);
        } 
        catch (IOException ex)
        {
            ex.printStackTrace();
        }
    }
    public String getOutfilePath()
    {
        return outfilePath;
    }
    
}
