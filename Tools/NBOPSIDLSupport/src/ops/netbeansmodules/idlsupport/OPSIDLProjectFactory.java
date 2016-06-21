/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package ops.netbeansmodules.idlsupport;

import configlib.XMLArchiverIn;

import configlib.XMLArchiverOut;
import configlib.exception.FormatException;
import java.io.File;
import java.io.FileInputStream;
//import java.io.FileOutputStream;
import java.io.IOException;
import ops.netbeansmodules.idlsupport.projectproperties.OPSProjectProperties;
import org.netbeans.api.project.Project;
import org.netbeans.spi.project.ProjectFactory;
import org.netbeans.spi.project.ProjectState;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;

/**
 *
 * @author angr
 */
//@org.openide.util.lookup.ServiceProvider(service=ProjectFactory.class)
public class OPSIDLProjectFactory implements ProjectFactory
{
    @Override
    public boolean isProject(FileObject projectDirectory)
    {
        return projectDirectory.getFileObject(OPSIDLProject.PROJECT_DIR) != null;
    }

    @Override
    public Project loadProject(FileObject dir, ProjectState state) throws IOException
    {
        if(isProject(dir))
        {
            OPSIDLProject project = new OPSIDLProject(dir, state);

            try
            {
                File inFile = new File(project.getProjectDirectory().getPath() + "/" + OPSIDLProject.PROJECT_DIR + "/" + OPSIDLProject.PROJECT_PROPFILE);
                XMLArchiverIn archiver = new XMLArchiverIn(new FileInputStream(inFile));
                archiver.add(OPSProjectProperties.getSerializableFactory());
                project.setProperties((OPSProjectProperties) archiver.inout("properties", project.getProperties()));
            }
            catch (FormatException ex)
            {
                //This is OK, we dont have a config yet
                project.setDefaultProperties();
            }
            catch (IOException ex)
            {
                Exceptions.printStackTrace(ex);
            }

            return project;
        }
        return null;
    }

    @Override
    public void saveProject(Project project) throws IOException, ClassCastException
    {
    }
}
