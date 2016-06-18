/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package ops.netbeansmodules.idlsupport;

import configlib.exception.FormatException;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Vector;
import ops.OPSConfig;
//import ops.netbeansmodules.idlsupport.compilers.CppCompiler;
//import ops.netbeansmodules.idlsupport.compilers.CsCompiler;
//import ops.netbeansmodules.idlsupport.compilers.DebugProjectCompiler;
//import ops.netbeansmodules.idlsupport.compilers.JavaCompiler;
//import ops.netbeansmodules.idlsupport.compilers.JavaOPSConfigCompiler;
//import ops.netbeansmodules.idlsupport.godegenerator.OPSConfigCompiler;
//import ops.netbeansmodules.idlsupport.compilers.VisualStudio2008CppExampleCompiler;
//import ops.netbeansmodules.idlsupport.godegenerator.OPSCodeGenerator;
import org.openide.filesystems.FileObject;
import org.openide.util.Exceptions;
import org.openide.util.Lookup;
//import parsing.IDLClass;
import org.openide.windows.InputOutput;
/**
 *
 * @author angr
 */
public class ProjectIDLCompiler
{

//    JavaCompiler javaCompiler = new JavaCompiler();
//    JavaOPSConfigCompiler javaOPSConfigCompiler = new JavaOPSConfigCompiler();
//    CsCompiler csCompiler = new CsCompiler();
//    CppCompiler cppCompiler = new CppCompiler();
//    DebugProjectCompiler debugProjectCompiler = new DebugProjectCompiler();
//    VisualStudio2008CppExampleCompiler cppExampleCompiler = new VisualStudio2008CppExampleCompiler();
    OPSIDLProject project;

    public ProjectIDLCompiler(OPSIDLProject project)
    {
        this.project = project;
    }

    void recursiveDelete(FileObject fileObject)
    {
        for (FileObject childFileObject : fileObject.getChildren())
        {
            if (!childFileObject.getName().equals(".svn") && !childFileObject.getName().equals(".cvs"))
            {
                if (childFileObject.isFolder())
                {
                    recursiveDelete(childFileObject);
                } else
                {
                    try
                    {
                        childFileObject.delete();
                    } catch (IOException ex)
                    {
                        Exceptions.printStackTrace(ex);
                    }
                }

            }
        }
    }

//    public void compile(Vector<IDLClass> idlClasses, InputOutput io)
//    {
//        //Test plugins
//
//        FileObject projectDirectory = project.getProjectDirectory();
//        for (FileObject fileObject : projectDirectory.getChildren())
//        {
//            if (fileObject.getName().equals("Generated"))
//            {
//                recursiveDelete(fileObject);
//            }
//        }
//        Collection<? extends OPSCodeGenerator> codeGenerators = Lookup.getDefault().lookupAll(OPSCodeGenerator.class);
//        for (OPSCodeGenerator codeGenerator : codeGenerators)
//        {
//            codeGenerator.compileDataClasses(idlClasses, project.getProjectDirectory().getPath() + "/Generated/");
//            codeGenerator.compileConfig(null, project.getProjectDirectory().getPath() + "/Generated/");
//            codeGenerator.build(project.getProjectDirectory().getPath() + "/Generated/");
//        }
//
//
//        if (project.getProperties().generateCpp)
//        {
//            io.getOut().println("Generating C++...");
//            cppCompiler.compileDataClasses(idlClasses, project.getProjectDirectory().getPath() + "/Generated/");
//            io.getOut().println("Done.");
//        }
//        if (project.getProperties().generateCS)
//        {
//            io.getOut().println("Generating C#...");
//            csCompiler.compileDataClasses(idlClasses, project.getProjectDirectory().getPath() + "/Generated/");
//            io.getOut().println("Done.");
//
//            if (project.getProperties().buildCS)
//            {
//                io.getOut().println("Building C#...");
//                try
//                {
//                    csCompiler.setDllDependencies(project.getProperties().csBuildDllDependencies);
//                    csCompiler.buildDll(project.getProjectDirectory().getPath(), io);
//                } catch (IOException ex)
//                {
//                    Exceptions.printStackTrace(ex);
//                } catch (InterruptedException ex)
//                {
//                    Exceptions.printStackTrace(ex);
//                }
//                io.getOut().println("Done.");
//            }
//        }
//        if (project.getProperties().generateJava)
//        {
//            io.getOut().println("Generating Java...");
//            javaCompiler.compileDataClasses(idlClasses, project.getProjectDirectory().getPath() + "/Generated/");
//
//            OPSConfig opsConfig;
//            try
//            {
//                File cfgFile = new File(project.getProjectDirectory().getPath() + "/" + project.getProperties().defaultOPSTopicConfigFile);
//                if (cfgFile.exists()) {
//                    opsConfig = OPSConfig.getConfig( cfgFile );
////                ArrayList<String> generatedFiles = javaOPSConfigCompiler.compileConfig(opsConfig, project.getProjectDirectory().getPath() + "/Generated/");
////
////                javaCompiler.appendFileToBuild(generatedFiles);
//
//                    Collection<? extends OPSConfigCompiler> configCompilers = Lookup.getDefault().lookupAll(OPSConfigCompiler.class);
//                    for (OPSConfigCompiler configCompiler : configCompilers)
//                    {
//                        ArrayList<String> generatedFiles = configCompiler.compileConfig(opsConfig, project.getProjectDirectory().getPath() + "/Generated/");
//                        javaCompiler.appendFileToBuild(generatedFiles);
//                    }
//                } else {
//                    io.getOut().println("Warn: Topic config file not found. Java Topic Config not generated.");
//                }
//            } catch (IOException ex)
//            {
//                ex.printStackTrace();
//                Exceptions.printStackTrace(ex);
//            } catch (FormatException ex)
//            {
//                ex.printStackTrace();
//                Exceptions.printStackTrace(ex);
//            }
//            io.getOut().println("Done.");
//
//            if (project.getProperties().buildJava)
//            {
//                io.getOut().println("Building Java...");
//                try
//                {
//                    javaCompiler.setJarDependencies(project.getProperties().javaBuildJarDependencies);
//                    javaCompiler.buildAndJar(project.getProjectDirectory().getPath(), io);
//                } catch (IOException ex)
//                {
//                    Exceptions.printStackTrace(ex);
//                } catch (InterruptedException ex)
//                {
//                    Exceptions.printStackTrace(ex);
//                }
//                io.getOut().println("Done.");
//            }
//
//        }
//        if (project.getProperties().buildDebugProject)
//        {
//            io.getOut().println("Generating Debug...");
//            debugProjectCompiler.createDebugProjectFile(project.getProjectDirectory().getPath(), project.getProjectDirectory().getName(), project.getProperties());
//            io.getOut().println("Done.");
//
//        }
//        if (Boolean.parseBoolean(project.getProperties().getPropertyValue("vsExampleEnabled", "false")))
//        {
//            io.getOut().println("Generating Visual C++ Examples...");
//            cppExampleCompiler.compileVSCppExample(project.getProjectDirectory().getPath(), project.getProjectDirectory().getName(), project.getProperties());
//            io.getOut().println("Done.");
//        }
//
//
//    }
}
