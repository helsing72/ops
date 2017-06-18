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
//import ops.netbeansmodules.idlsupport.projectproperties.JarDependency;
import opsc.ProjectProperties;
import parsing.AbstractTemplateBasedIDLCompiler;
import parsing.IDLField;
import parsing.IDLClass;

/**
 *
 * @author angr
 */
public class DebugProjectCompiler extends opsc.Compiler
{
    final static String DOMAIN_ID_REGEX = "__DomainID";
    final static String JAR_PATHS_REGEX = "__jarPaths";

    public DebugProjectCompiler(String projectName) {
        super(projectName);
    }

    public void compileDataClasses(Vector<IDLClass> arg0, String arg1)
    {
        throw new UnsupportedOperationException("Not supported yet.");
    }

    public void createDebugProjectFile(String projectDirectory, String projectName, ProjectProperties projProps)
    {
        try {
            setOutputFileName(projectDirectory + File.separator + "DebugProject.xml");
            System.out.println("Creating Debug Project File: " + outputFileName);

            java.io.InputStream stream = findTemplateFile("debugproj.tpl");
            setTemplateTextFromResource(stream);

            setTabString("    ");

            String result = getTemplateText();

            result = result.replaceAll(DOMAIN_ID_REGEX, projProps.debugProjDomainID);
            result = result.replaceAll(JAR_PATHS_REGEX, createJarPaths(projProps, projectName));

            saveOutputText(result);
        } catch (IOException ex)
        {
            System.out.println("Generating Debug Project File failed with the following exception: " + ex.getMessage());
        }

    }

    public String getName()
    {
        return "DebugProjectCompiler";
    }

    private String createJarPaths(ProjectProperties projProps, String projectName)
    {
        String ret = "";
        ret += "<path>Java/" + projectName + ".jar</path>" + endl();

        for (JarDependency jarDependency : projProps.javaBuildJarDependencies)
        {
            ret += "<path>" + jarDependency.path + "</path>" + endl();
        }

        return ret;
    }

    public void compileDataClass(IDLClass idlClass) throws IOException {}
    protected void compileEnum(IDLClass idlClass) throws IOException {}
    protected void compilePublisher(IDLClass idlClass) throws IOException {}
    protected void compileSubscriber(IDLClass idlClass) throws IOException {}
    protected void compileTypeSupport(Vector<IDLClass> idlClasses, String projectName) {}
    protected String getConstructorBody(IDLClass idlClass) { return ""; }
    protected String getDeclarations(IDLClass idlClass) { return ""; }
    protected String getDeclareVector(IDLField field) { return ""; }
    protected String languageType(String s) { return ""; }
    protected String getSerialize(IDLClass idlClass) { return ""; }

}
