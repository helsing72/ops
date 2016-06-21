/*
 * JavaTopicConfigCreator.java
 *
 * Created on den 24 oktober 2007, 10:15
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor.
 */

package idlcompiler.topicconfigcreators;

import idlcompiler.compilers.Compiler;
import idlcompiler.files.Project;
import java.io.File;
import java.io.IOException;
import ops.StaticManager;
import ops.Topic;
import org.xml.sax.SAXException;
import parsing.TopicInfo;
import util.FileHelper;

/**
 *
 * @author angr
 */
public class JavaTopicConfigCreator
{

    private String outfilePath = "";
    
    private static String CLASSNAME_TEXT = "__className";
    private static String PACKAGENAME_TEXT = "__packageName";
    private static String REPLYTOPIC_TEXT = "__replyTopic";
    private static String REQUESTTOPIC_TEXT = "__requestTopic";
    private static String REPLYTYPE_TEXT = "__replyType";
    private static String REQUESTTYPE_TEXT = "__requestType";
    private static String REQUESTTYPE_NOPACKAGE_TEXT = "__requestNoPackage";
    private static String REPLYTYPE_NOPACKAGE_TEXT = "__replyNoPackage";
    private static String TOPICCONFIGCLASS_TEXT = "__topicConfigClass";
    
    
    /**
     * Creates a new instance of JavaTopicConfigCreator
     */
    public JavaTopicConfigCreator(Project project, String packageString) throws SAXException, IOException
    {
        String configFileString = "file:///" + project.getRunDirectory().replace("\\", "/") + "/" + project.getTopicConfigFile().getFile().getName();
        XMLConfigLoader configLoader = new XMLConfigLoader(configFileString);
        
        generateTopicConfigClass(project, packageString, configLoader);
        
        generateRequestReplyClasses(project, packageString, configLoader);
        
    }

    public String getOutfilePath()
    {
        return outfilePath;
    }

    private void generateRequestReplyClasses(Project project, String packageString, XMLConfigLoader configLoader) throws IOException
    {
        for (RequestReplyInfo reqRepInfo : configLoader.getRequestReplyInfos())
        {
            String requestFileText = FileHelper.getTextFileText("javarequesttemplate.tpl");
            String replyFileText = FileHelper.getTextFileText("javareplytemplate.tpl");

            requestFileText = requestFileText.replace(CLASSNAME_TEXT, reqRepInfo.className);
            requestFileText = requestFileText.replace(PACKAGENAME_TEXT, packageString);
            
            replyFileText = replyFileText.replace(CLASSNAME_TEXT, reqRepInfo.className);
            replyFileText = replyFileText.replace(PACKAGENAME_TEXT, packageString);
            
            
            replyFileText = replyFileText.replace(REPLYTOPIC_TEXT, reqRepInfo.replyTopic);
            requestFileText = requestFileText.replace(REPLYTOPIC_TEXT, reqRepInfo.replyTopic);
            
            requestFileText = requestFileText.replace(REQUESTTOPIC_TEXT, reqRepInfo.requestTopic);
            replyFileText = replyFileText.replace(REQUESTTOPIC_TEXT, reqRepInfo.requestTopic);
            
            String replyType = configLoader.getTypeForTopic(reqRepInfo.replyTopic);
            requestFileText = requestFileText.replace(REPLYTYPE_TEXT, replyType);
            replyFileText = replyFileText.replace(REPLYTYPE_TEXT, replyType);
            
            String requestType = configLoader.getTypeForTopic(reqRepInfo.requestTopic);
            requestFileText = requestFileText.replace(REQUESTTYPE_TEXT, requestType);
            replyFileText = replyFileText.replace(REQUESTTYPE_TEXT, requestType);
            
            
            
            String topicConfigClass = project.getName() + "TopicConfig";
            requestFileText = requestFileText.replace(TOPICCONFIGCLASS_TEXT, topicConfigClass);
            replyFileText = replyFileText.replace(TOPICCONFIGCLASS_TEXT, topicConfigClass);
            
            String requestOutdirPath = FileHelper.unixSlashed(project.getRunDirectory()) + "/" + project.getRelativeOutputPath() + "/" + "Java/" + packageString.replace(".", "/") + "/";
            new File(requestOutdirPath).mkdirs();
            requestOutdirPath = requestOutdirPath + "/" + reqRepInfo.className + "Proxy.java";

            FileHelper.createAndWriteFile(requestOutdirPath, requestFileText);
            
            String replyOutdirPath = FileHelper.unixSlashed(project.getRunDirectory()) + "/" + project.getRelativeOutputPath() + "/" + "Java/" + packageString.replace(".", "/") + "/";
            new File(replyOutdirPath).mkdirs();
            replyOutdirPath = replyOutdirPath + "/" + reqRepInfo.className + "Stub.java";

            FileHelper.createAndWriteFile(replyOutdirPath, replyFileText);
            
            outfilePath += "\"" + replyOutdirPath +"\"\n";
            outfilePath += "\"" + requestOutdirPath +"\"\n";
            

        }
    }

    private void generateTopicConfigClass(Project project, String packageString, XMLConfigLoader configLoader)
    {

        String output = "";
        output += "package " + packageString + ";\n";
        String className = project.getName().replace(".prj", "") + "TopicConfig";
        output += "public class " + className + "\n";
        output += "{\n";
        output += "\tprivate String domainAddress = null;\n";
        output += "\tpublic " + className + "(String domainAddress)\n";
        output += "\t{\n";
        output += "\t\tthis.domainAddress = domainAddress;\n";
        output += "\t}\n";

        for (TopicInfo t : configLoader.getTopics())
        {
            output += "\tpublic ops.Topic<" + t.type + "> get" + t.name + "()\n";
            output += "\t{\n";
            output += "\t\treturn new ops.Topic<" + t.type + ">(\"" + t.name + "\", " + t.port + ", \"" + t.type + "\", this.domainAddress);\n";
            output += "\t}\n";
        }
        output += "}\n";
        try
        {
            String outdirPath = project.getRunDirectory().replace("\\", "/") + "/" + project.getRelativeOutputPath() + "/" + "Java/" + packageString.replace(".", "/") + "/";
            new File(outdirPath).mkdirs();
            String tempOutfilePath = outdirPath + "/" + FileHelper.cropExtension(project.getName()) + "TopicConfig.java";

            Compiler.createAndWriteFile(tempOutfilePath, output);
            outfilePath += "\"" + tempOutfilePath + "\"\n";

//            String execString = "javac -cp OPSJLib/OPSJLib.jar \"" + getOutfilePath() + "\"";
//            Runtime.getRuntime().exec(execString);
        } catch (IOException ex)
        {
            ex.printStackTrace();
        }
    }
    
}
