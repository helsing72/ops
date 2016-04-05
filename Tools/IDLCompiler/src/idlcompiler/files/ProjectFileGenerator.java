/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package idlcompiler.files;

import java.io.File;
import java.io.IOException;
import util.FileHelper;

/**
 *
 * @author angr
 */
public class ProjectFileGenerator
{
    public static void generateAndSave(Project theProject) throws IOException
    {
        String text = "";
        text += "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n";
        
        text += "<OPSProject name = \"" + FileHelper.cropExtension(theProject.getName()) + "\">\n";
        
        text +=     "\t<Options>\n";
        text +=         "\t\t<Editor highLighting = \"" + theProject.getCodeLightingEnabled() + "\"/>\n";
        text +=         "\t\t<TopicConfig filePath = \"" + theProject.getTopicConfigFile().getName() +"\" packageName = \"" + theProject.getTopicConfigPackage() + "\"/>\n";
        text +=         "\t\t<CodeGeneration outputDirectory = \"" + theProject.getRelativeOutputPath() + "\" generateJava = \"" + theProject.isJavaGenerationEnabled() + "\"  generateCs = \"" + theProject.isCsGenerationEnabled() + "\" generateCpp = \"" + theProject.isCppGenerationEnabled() + "\"/>\n";
        text +=         "\t\t<JavaBuild enabled = \"" + theProject.isBuildAndJarEnabled() + "\">\n";
        for (String jarDepString : theProject.getJarDependencies())
        {
            text +=         "\t\t\t<JarDependency path = \"" + jarDepString.replace("\\", "/") + "\"/>\n";
        }
        text +=         "\t\t</JavaBuild>\n";
        text +=     "\t</Options>\n";
        
        for (IDLFile idlFile  : theProject.getIDLFiles())
        {
            text += "\t<IDLFile fileName = \"" + idlFile.getRelativPath() + idlFile.getName() + "\"/>\n";
            
        }
 
        text += "</OPSProject>\n";
        
        FileHelper.createAndWriteFile(theProject.getRunDirectory() + File.separator + FileHelper.cropExtension(theProject.getName()) + ".prj", text);
        
        
        
    }
}
