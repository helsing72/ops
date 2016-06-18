/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package idlcompiler.compilers;

import java.util.Vector;
import parsing.AbstractTemplateBasedIDLCompiler;
import parsing.IDLClass;
import parsing.TopicInfo;

/**
 *
 * @author angr
 */
public class CppFactoryIDLCompiler extends AbstractTemplateBasedIDLCompiler//implements IDLCompiler
{
    final static String PROJECT_NAME_REGEX = "__projectName";
    final static String TOPIC_CONFIG_NAME_REGEX = "__topicConfig";
    final static String CREATE_SUBSCRIBER_BODY_REGEX = "__createSubscriberBody";
    final static String CREATE_PUBLISHER_BODY_REGEX = "__createPublisherBody";
    final static String CREATE_HELPER_BODY_REGEX = "__createHelperBody";

    Vector<IDLClass> idlClasses;

    private Vector<TopicInfo> topics;
    private String factoryName;
    private String projectDirectory;
    private String packageString;
    private String projectName;

    public void compileDataClasses(Vector<IDLClass> idlClasses, String projectDirectory)
    {
        this.idlClasses = idlClasses;
    }

    public void compileTopicConfig(Vector<TopicInfo> topics, String name, String packageString, String projectDirectory)
    {
        this.topics = topics;
        this.projectDirectory = projectDirectory;
        this.packageString = packageString;
        this.projectName = name;
        factoryName = projectName + "OPSFactory";

        setOutputFileName(projectDirectory + "/C++/" + packageString + "/" + factoryName + ".h");
        setTemplateFileName("CppOPSFactorytemplate.tpl");
        setTabString("    ");//Default is "\t"
        setEndlString("\r\n");//Default is "\r\n"

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, factoryName);
        templateText = templateText.replace(PROJECT_NAME_REGEX, projectName);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageString);
        templateText = templateText.replace(TOPIC_CONFIG_NAME_REGEX, packageString + "::" + name + "TopicConfig");

        templateText = templateText.replace(CREATE_SUBSCRIBER_BODY_REGEX, getCreateSubscriberBody());
        templateText = templateText.replace(CREATE_PUBLISHER_BODY_REGEX, getCreatePublisherBody());
        templateText = templateText.replace(INCLUDES_REGEX, getIncludes());

        //Save the modified text to the output file.
        saveOutputText(templateText);


    }

    public String getName()
    {
        return "CppFactoryIDLCompiler";
    }

    private String getCreatePublisherBody()
    {
        String ret = tab(3) + "ops::Publisher* pub = NULL;" + endl();
        for (TopicInfo topic : topics)
        {
            ret += tab(3) + "if(topicName == \"" + topic.name +  "\")" + endl();
            ret += tab(3) + "{" + endl();
            ret += tab(4) +     "pub = new " + topic.type.replace(".", "::") + "Publisher(topicConfig.get" + topic.name + "()" + ");" + endl();
            ret += tab(3) + "}" + endl();

        }
        //ret += tab(3) + "return NULL;" + endl();
        return ret;
    }

    private String getCreateSubscriberBody()
    {
        String ret = tab(3) + "ops::Subscriber* sub = NULL;" + endl();
        for (TopicInfo topic : topics)
        {
            ret += tab(3) + "if(topicName == \"" + topic.name +  "\")" + endl();
            ret += tab(3) + "{" + endl();
            ret += tab(4) +     "sub = new " + topic.type.replace(".", "::") + "Subscriber(topicConfig.get" + topic.name + "()" + ");" + endl();
            ret += tab(3) + "}" + endl();

        }
        //ret += tab(3) + "return NULL;" + endl();
        return ret;
        
    }

    private String getIncludes()
    {
        String ret = "";
        ret += tab(0) + "#include \"" + packageString + "/" + projectName + "TopicConfig.h\"" + endl();
        for (TopicInfo topic : topics)
        {
            ret += tab(0) + "#include \"" + topic.type.replace(".", "/") + "Subscriber.h\"" + endl();
            ret += tab(0) + "#include \"" + topic.type.replace(".", "/") + "Publisher.h\"" + endl();

        }

        return ret;
    }
}
