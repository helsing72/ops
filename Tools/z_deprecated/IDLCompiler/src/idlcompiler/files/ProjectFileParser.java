/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package idlcompiler.files;

import java.io.File;
import java.io.IOException;
import java.util.Collections;
import java.util.Comparator;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;
import util.FileHelper;

/**
 *
 * @author angr
 */
public class ProjectFileParser extends DefaultHandler
{
    Project project;
    public ProjectFileParser()
    {
        super();
        
       
        
    }
    
    public Project parse(String fileName) throws SAXException, IOException
    {
//        if(!fileName.startsWith("file://"))
//        {
//            fileName = "file://" + fileName;
//        }
        
        project = new Project(null, new File(fileName).getParent());
        
        XMLReader xr = XMLReaderFactory.createXMLReader();
                       
        
        xr.setContentHandler(this);
	xr.setErrorHandler(this);
        
        xr.parse(new InputSource("file:///" + fileName));
        
        Comparator comp = new Comparator() 
        {

            public int compare(Object o1, Object o2)
            {
                //return Collections.reverseOrder().compare(o1, o2);
                return o1.toString().compareTo(o2.toString());
            }
        };

       Collections.sort(project.getIDLFiles(), comp);
       //Collections.reverse(project.getIDLFiles());
        
        
        return project;
        
        
    }
    
    ////////////////////////////////////////////////////////////////////
    // Event handlers.
    ////////////////////////////////////////////////////////////////////

    public void startElement (String uri, String name,
			      String qName, Attributes atts)
    {
        if(name.equals("OPSProject"))
        {
            project.name = atts.getValue("name");
        }
        else if(name.equals("Editor"))
        {
            project.enableCodeHighLighting(Boolean.parseBoolean(atts.getValue("highLighting")));            
        }
        else if (name.equals("TopicConfig"))
        {
            try
            {
                project.setTopicConfig(new TopicConfigFile(new File(project.getRunDirectory() + "/" + atts.getValue("filePath"))));
                project.setTopicConfigPackage(atts.getValue("packageName"));
            } 
            catch (IOException ex)
            {
                project.setInvalid("Your topic config settings contain an error.\n");
            }
            
        }
        else if (name.equals("CodeGeneration"))
        {
            project.setRelativeOutputPath(atts.getValue("outputDirectory"));
            project.enableJavaGeneration(Boolean.parseBoolean(atts.getValue("generateJava")));
            project.enableCsGeneration(Boolean.parseBoolean(atts.getValue("generateCs")));
            project.enableCppGeneration(Boolean.parseBoolean(atts.getValue("generateCpp")));
            
        }
        else if (name.equals("JavaBuild"))
        {
            project.enableBuildAndJar(Boolean.parseBoolean(atts.getValue("enabled")));
        }
        else if (name.equals("JarDependency"))
        {
            project.addJarDependency(atts.getValue("path"));      
        }
        else if (name.equals("IDLFile"))
        {
            File f = new File(project.getRunDirectory() + "/" + atts.getValue("fileName"));
            project.addFile(new IDLFile(f, project.getRunDirectory(), FileHelper.getRelativePath(new File(project.getRunDirectory()), f.getParentFile()) + "/"));
            
        }
        
    }
    
    public void startDocument ()
    {
	
    }

    public void endDocument ()
    {
	
    }

    public void endElement (String uri, String name, String qName)
    {
	
    }


    public void characters (char ch[], int start, int length)
    {
	
    }
        
}

