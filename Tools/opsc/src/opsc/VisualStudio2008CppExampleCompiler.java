/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package opsc;

import java.io.IOException;
import java.io.InputStream;
import java.io.File;
import java.util.Vector;
import javax.swing.JOptionPane;
import opsc.ProjectProperties;
import parsing.AbstractTemplateBasedIDLCompiler;
import parsing.IDLField;
import parsing.IDLClass;
import parsing.TopicInfo;

/**
 *
 * @author angr
 */
public class VisualStudio2008CppExampleCompiler extends opsc.Compiler
{
    public static String PROJ_NAME_REGEX = "__projectName";
    public static String TOPIC_NAME_REGEX = "__topicName";
    public static String DATA_TYPE_REGEX = "__dataType";
    public static String DOMAIN_ID_REGEX = "__domainName";
    public static String INCLUDE_DATA_TYPE_PATH_REGEX = "__includeDataTypePath";

    public VisualStudio2008CppExampleCompiler(String projectName) {
        super(projectName);
    }

    public void compileVSCppExample(String projectDirectory, String projectName, ProjectProperties projProps)
    {
        String vsExampleTopicName = projProps.getPropertyValue("vsExampleTopicName", "");
        String basePath = projectDirectory + File.separator + "Visual Studio Examples" + File.separator;
        String subCppFile = vsExampleTopicName + "_sub.cpp";
        String pubCppFile = vsExampleTopicName + "_pub.cpp";
        String subProjFile = vsExampleTopicName + "_sub.vcproj";
        String pubProjFile = vsExampleTopicName + "_pub.vcproj";
        String slnFile = vsExampleTopicName + "_example.sln";
        String opsConfigPath = "";

        try {
            System.out.println("Creating VS Example in directory: " + basePath);
            createFile(basePath, slnFile, "vs_sln.tpl", projectName, projProps);
            createFile(basePath, pubProjFile, "vs_pub_proj.tpl", projectName, projProps);
            createFile(basePath, subProjFile, "vs_sub_proj.tpl", projectName, projProps);
            createFile(basePath, pubCppFile, "vs_pub_cpp.tpl", projectName, projProps);
            createFile(basePath, subCppFile, "vs_sub_cpp.tpl", projectName, projProps);
        } catch (IOException iOException)
        {
            System.out.println("Generating Visual Studio Example failed with the following exception: " + iOException.getMessage());
        }
    }

    public void compileDataClasses(Vector<IDLClass> arg0, String arg1)
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void compileTopicConfig(Vector<TopicInfo> arg0, String arg1, String arg2, String arg3)
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public String getName()
    {
        return "VisualStudio2008CppExampleCompiler";
    }

    private void createFile(String basePath, String outFileName, String tplFileName, String projectName, ProjectProperties projProps) throws IOException
    {
        setOutputFileName(basePath + outFileName);
        System.out.println("  File: " + outFileName);

        java.io.InputStream stream = findTemplateFile(tplFileName);
        setTemplateTextFromResource(stream);

        setTabString("    ");

        String vsExampleTopicName = projProps.getPropertyValue("vsExampleTopicName", "");
        String vsExampleDataType = projProps.getPropertyValue("vsExampleDataType", "");
        String vsExampleDomainID = projProps.getPropertyValue("vsExampleDomainID", "");

        String result = getTemplateText();
        result = result.replaceAll(TOPIC_NAME_REGEX, vsExampleTopicName);
        result = result.replaceAll(DATA_TYPE_REGEX, vsExampleDataType.replace(".", "::"));
        result = result.replaceAll(INCLUDE_DATA_TYPE_PATH_REGEX, vsExampleDataType.replace(".", "/"));
        result = result.replaceAll(DOMAIN_ID_REGEX, vsExampleDomainID);
        result = result.replaceAll(PROJ_NAME_REGEX, projectName);
        saveOutputText(result);
    }

    public void compileDataClass(IDLClass idlClass) throws IOException {}
    protected void compileEnum(IDLClass idlClass) throws IOException {}
    protected void compilePublisher(IDLClass idlClass) throws IOException {}
    protected void compileSubscriber(IDLClass idlClass) throws IOException {}
    protected void compileTypeSupport(Vector<IDLClass> idlClasses, String projectName) throws IOException {}
    protected String getConstructorBody(IDLClass idlClass) { return ""; }
    protected String getDeclarations(IDLClass idlClass) { return ""; }
    protected String getDeclareVector(IDLField field) { return ""; }
    protected String languageType(String s) { return ""; }
    protected String getSerialize(IDLClass idlClass) { return ""; }

}
