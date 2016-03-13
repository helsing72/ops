/*
 * NewJavaCompiler.java
 *
 * Created on den 12 november 2007, 15:39
 *
 * To change this template, choose Tools | Template Manager
 * and open the template in the editor. 
 */
package opsc;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.UnsupportedEncodingException;
import java.lang.ClassLoader;
import java.util.List;
import java.util.Vector;
import parsing.AbstractTemplateBasedIDLCompiler;
import parsing.IDLClass;
import parsing.IDLField;
import parsing.TopicInfo;

/**
 *
 * @author angr
 */
public class JavaCompiler extends opsc.Compiler
{
    final static String CONSTRUCTOR_BODY_REGEX = "__constructorBody";
    final static String DECLARATIONS_REGEX = "__declarations";
    final static String SERIALIZE_REGEX = "__serialize";
    final static String DESERIALIZE_REGEX = "__deserialize";
    final static String CLONE_BODY_REGEX = "__cloneBody";
    final static String SIZE_REGEX = "__size";
    final static String JAVA_DIR = "Java";
    //Vector<IDLClass> idlClasses;
    //private String projectDirectory;
    private static String BASE_CLASS_NAME_REGEX = "__baseClassName";
    private static String CREATE_BODY_REGEX = "__createBody";
    String createdFiles = "";
    private Vector<JarDependency> jarDependencies;
    //private String _projectName;

    public JavaCompiler(String projname) {
        super(projname);
    }
    
    public void compileDataClasses(Vector<IDLClass> idlClasses, String projectDirectory)
    {
        createdFiles = "";
        _idlClasses = idlClasses;
        //this.projectDirectory = projectDirectory;
        try
        {
            for (IDLClass iDLClass : _idlClasses)
            {
                if (iDLClass.getType() == IDLClass.ENUM_TYPE)
                {
                    System.out.println("Compile enum");
                    compileEnum(iDLClass);
                }
                else
                {
                    compileDataClass(iDLClass);
                    compileSubscriber(iDLClass);
                    compilePublisher(iDLClass);
                }
            }
            compileTypeSupport(idlClasses, _projectName);
        }
        catch (IOException iOException)
        {
            System.out.println("Generating Java failed with the following exception: " + iOException.getMessage());
        }

    }

    public void compileTopicConfig(Vector<TopicInfo> topics, String name, String packageString, String projectDirectory)
    {
    }

    protected void compileEnum(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();

        String packageName = idlClass.getPackageName();
        String packageFilePart = packageName.replace(".", "/");
        //setOutputFileName(projectDirectory + JAVA_DIR + "/" + packageFilePart + "/" + className + ".java");
        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + className + ".java");

        //String resource = "/ops/netbeansmodules/idlsupport/templates/javaenumtemplate.tpl";
        String resource = findTemplateFile("javaenumtemplate.tpl");
        setTemplateTextFromResource(resource);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(DECLARATIONS_REGEX, getEnumDeclarations(idlClass));

        //Save the modified text to the output file.
        saveOutputText(templateText);
        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    public void compileDataClass(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();
        String baseClassName = "OPSObject";
        if (idlClass.getBaseClassName() != null)
        {
            baseClassName = idlClass.getBaseClassName();
        }
        String packageName = idlClass.getPackageName();
        String packageFilePart = packageName.replace(".", "/");

        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + className + ".java");

        String resource = findTemplateFile("javatemplate.tpl");

        setTemplateTextFromResource(resource);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(BASE_CLASS_NAME_REGEX, baseClassName);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);
        templateText = templateText.replace(CONSTRUCTOR_BODY_REGEX, getConstructorBody(idlClass));
        templateText = templateText.replace(DECLARATIONS_REGEX, getDeclarations(idlClass));
        templateText = templateText.replace(SERIALIZE_REGEX, getSerialize(idlClass));
        templateText = templateText.replace(CLONE_BODY_REGEX, getCloneBody(idlClass));

        //Save the modified text to the output file.
        saveOutputText(templateText);
        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    public String getName()
    {
        return "JavaCompiler";
    }

    protected void compilePublisher(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");

        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + className + "Publisher.java");

        String resource = findTemplateFile("javapublishertemplate.tpl");

        setTemplateTextFromResource(resource);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);

        //Save the modified text to the output file.
        saveOutputText(templateText);

        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    protected void compileSubscriber(IDLClass idlClass) throws IOException
    {
        String className = idlClass.getClassName();
        String packageName = idlClass.getPackageName();

        String packageFilePart = packageName.replace(".", "/");

        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + className + "Subscriber.java");

        String resource = findTemplateFile("javasubscribertemplate.tpl");
        setTemplateTextFromResource(resource);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);

        //Save the modified text to the output file.
        saveOutputText(templateText);

        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    protected void compileTypeSupport(Vector<IDLClass> idlClasses, String projectName) throws IOException
    {
        String className = projectName + "TypeFactory";
        String packageName = projectName;

        String packageFilePart = packageName.replace(".", "/");

        setOutputFileName(_outputDir + File.separator + packageFilePart + File.separator + className + ".java");

        String resource = findTemplateFile("javatypefactorytemplate.tpl");
        setTemplateTextFromResource(resource);

        //Get the template file as a String
        String templateText = getTemplateText();

        //Replace regular expressions in the template file.
        templateText = templateText.replace(CLASS_NAME_REGEX, className);
        templateText = templateText.replace(PACKAGE_NAME_REGEX, packageName);

        //Save the modified text to the output file.
        String createBodyText = "";

        for (IDLClass iDLClass : idlClasses)
        {
            createBodyText += tab(2) + "if(type.equals(\"" + iDLClass.getPackageName() + "." + iDLClass.getClassName() + "\"))" + endl();
            createBodyText += tab(2) + "{" + endl();
            createBodyText += tab(3) + "return new " + iDLClass.getPackageName() + "." + iDLClass.getClassName() + "();" + endl();
            createBodyText += tab(2) + "}" + endl();
        }
        createBodyText += tab(2) + "return null;" + endl();

        templateText = templateText.replace(CREATE_BODY_REGEX, createBodyText);

        saveOutputText(templateText);

        createdFiles += "\"" + getOutputFileName() + "\"\n";
    }

    protected String getConstructorBody(IDLClass idlClass)
    {
        String ret = "";
        return ret;
    }
    
    protected String getCloneBody(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields())
        {
            if (field.isIdlType())
            {
                if (!field.isArray())
                {
                    ret += tab(2) + "cloneResult." +  field.getName() + " = (" + field.getType() + ")this." + field.getName() + ".clone();" + endl();
                }
                else
                {
                    ret += tab(2) + "java.util.Collections.copy(" + "cloneResult." +  field.getName() + ", this." + field.getName() + ");" + endl();

                }
            }

            //"Arrays.copyOf(original, newLength)";
            else if (field.isArray())
            {

                ret += tab(2) + "java.util.Collections.copy(" + "cloneResult." +  field.getName() + ", this." + field.getName() + ");" + endl();

            }
            else
            {
                ret += tab(2) + "cloneResult." +  field.getName() + " = this." + field.getName() + ";" + endl();
            }

        }
        return ret;
        
    }



    private String getEnumDeclarations(IDLClass idlClass)
    {
        String ret = "";
        for (int i = 0; i < idlClass.getEnumNames().size(); i++)
        {
            ret += idlClass.getEnumNames().get(i);
            ret += ",";
        }
        return ret;
    }

    protected String getDeclarations(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields())
        {
            if (!field.getComment().equals(""))
            {
                String comment = field.getComment();
                int idx;
                while ((idx = comment.indexOf('\n')) >= 0) {
                  ret += tab(1) + "///" + comment.substring(0,idx).replace("/*", "").replace("*/", "") + endl();
                  comment = comment.substring(idx+1);
                }
                ret += tab(1) + "///" + comment.replace("/*", "").replace("*/", "") + endl();
//                ret += tab(1) + "///" + field.getComment().replace("/*", "").replace("*/", "") + endl();
//                ret += tab(1) + "///" + field.getComment() + endl();
            }
            if (field.isArray())
            {
                ret += tab(1) + "public " + getDeclareVector(field);
            }
            else if (field.getType().equals("string"))
            {
                ret += tab(1) + "public " + languageType(field.getType()) + " " + field.getName() + " = \"\";" + endl();
            }
            else if (field.isIdlType())
            {
                ret += tab(1) + "public " + languageType(field.getType()) + " " + field.getName() + " = new " + languageType(field.getType()) + "();" + endl();
            }
            else //Simple primitive type
            {
                ret += tab(1) + "public " + languageType(field.getType()) + " " + field.getName() + ";" + endl();
            }

        }
        return ret;
    }

    protected String getDeclareVector(IDLField field)
    {
        return languageType(field.getType()) + " " + field.getName() + " = new " + languageType(field.getType()) + "();" + endl();
    }

    protected String languageType(String s)
    {
        if (s.equals("string"))
        {
            return "String";
        }
        else if (s.equals("boolean"))
        {
            return "boolean";
        }
        else if (s.equals("int"))
        {
            return "int";
        }
        else if (s.equals("long"))
        {
            return "long";
        }
        else if (s.equals("double"))
        {
            return "double";
        }
        else if (s.equals("float"))
        {
            return "float";
        }
        else if (s.equals("byte"))
        {
            return "byte";
        }
        else if (s.equals("string[]"))
        {
            return "java.util.Vector<String>";
        }
        else if (s.equals("int[]"))
        {
            return "java.util.Vector<Integer>";
        }
        else if (s.equals("long[]"))
        {
            return "java.util.Vector<Long>";
        }
        else if (s.equals("double[]"))
        {
            return "java.util.Vector<Double>";
        }
        else if (s.equals("float[]"))
        {
            return "java.util.Vector<Float>";
        }
        else if (s.equals("byte[]"))
        {
            return "java.util.Vector<Byte>";
        }
        else if (s.equals("boolean[]"))
        {
            return "java.util.Vector<Boolean>";
        }
        else if (s.endsWith("[]"))
        {
            return "java.util.Vector<" + s.substring(0, s.indexOf('[')) + ">";
        }
        else if (s.equals("static string"))
        {
            return "final static String";
        }
        return s;

    }

    protected String getSerialize(IDLClass idlClass)
    {
        String ret = "";
        for (IDLField field : idlClass.getFields())
        {
            if (field.isIdlType())
            {
                if (!field.isArray())
                {
                    ret += tab(2) + field.getName() + " = (" + field.getType() + ") archive.inout(\"" + field.getName() + "\", " + field.getName() + ");" + endl();
                }
                else
                {
                    ret += tab(2) + field.getName() + " = (" + languageType(field.getType()) + ") archive.inoutSerializableList(\"" + field.getName() + "\", " + field.getName() + ");" + endl();

                }
            }
            else if (field.isArray())
            {
                if (field.getType().equals("int[]"))
                {
                    ret += tab(2) + field.getName() + " = (" + languageType(field.getType()) + ") archive.inoutIntegerList(\"" + field.getName() + "\", " + field.getName() + ");" + endl();
                }
                else if (field.getType().equals("byte[]"))
                {
                    ret += tab(2) + field.getName() + " = (" + languageType(field.getType()) + ") archive.inoutByteList(\"" + field.getName() + "\", " + field.getName() + ");" + endl();
                }
                else if (field.getType().equals("long[]"))
                {
                    ret += tab(2) + field.getName() + " = (" + languageType(field.getType()) + ") archive.inoutLongList(\"" + field.getName() + "\", " + field.getName() + ");" + endl();
                }
                else if (field.getType().equals("boolean[]"))
                {
                    ret += tab(2) + field.getName() + " = (" + languageType(field.getType()) + ") archive.inoutBooleanList(\"" + field.getName() + "\", " + field.getName() + ");" + endl();
                }
                else if (field.getType().equals("float[]"))
                {
                    ret += tab(2) + field.getName() + " = (" + languageType(field.getType()) + ") archive.inoutFloatList(\"" + field.getName() + "\", " + field.getName() + ");" + endl();
                }
                else if (field.getType().equals("double[]"))
                {
                    ret += tab(2) + field.getName() + " = (" + languageType(field.getType()) + ") archive.inoutDoubleList(\"" + field.getName() + "\", " + field.getName() + ");" + endl();
                }
                else if (field.getType().equals("string[]"))
                {
                    ret += tab(2) + field.getName() + " = (" + languageType(field.getType()) + ") archive.inoutStringList(\"" + field.getName() + "\", " + field.getName() + ");" + endl();
                }
            }
            else
            {
                ret += tab(2) + field.getName() + " = archive.inout(\"" + field.getName() + "\", " + field.getName() + ");" + endl();
            }

        }
        return ret;
    }

    public void setJarDependencies(Vector<JarDependency> jarDeps)
    {
        this.jarDependencies = jarDeps;

    }

    public void buildAndJar(String projectDir) throws IOException, InterruptedException
    {
        // don't want to build bat scripts on linux...
        final boolean isLinux = System.getProperty("os.name").equals("Linux");
        
        String jarPackString = null;

        String jarDepString = "";
        String manifestJarDepString = "Class-Path: ";

        if(jarDependencies != null) {
            for (JarDependency jarDep : jarDependencies)
            {
                File jarToBeCopied = new File(jarDep.path + "");
                if (!jarToBeCopied.isAbsolute()) {
                    jarToBeCopied = new File(projectDir + "/" + jarDep.path + "");
                    jarDepString += "\"" + projectDir + "/" + jarDep.path + "\";";
                } else {
                    jarDepString += "\"" + jarDep.path + "\";";
                }
                File jarCopy = new File(projectDir + "/" + "Generated" + "/" + jarToBeCopied.getName());
                jarCopy.createNewFile();

                System.out.println("HELM remove cp " + jarToBeCopied + " -> " + jarCopy);
                //FileHelper.copyFile(jarToBeCopied, jarCopy);

                manifestJarDepString += jarCopy.getName() + " ";
            }
//        manifestJarDepString += "\nTopic-config: " + theProject.getTopicConfigPackage() + "." + theProject.getName() + "TopicConfig";
            manifestJarDepString += endl();
        } else {
            System.out.println("WARN: no jar dependencies");
        }
        
        String dinfoPath = _outputDir + File.separator + "debugger_buildinfo.ops_tmp";
        createAndWriteFile(dinfoPath, createdFiles);

        String manFilePath = _outputDir + File.separator + "/manifest_adds.ops_tmp";
        createAndWriteFile(manFilePath, manifestJarDepString);

        /// Try to find out the full path for the included OPS Jar files
        String ExePath = "";
        String SubPath = "";
        try {
          ExePath = JavaCompiler.class.getProtectionDomain().getCodeSource().getLocation().getPath();
          ExePath = java.net.URLDecoder.decode(ExePath, "UTF-8");
          SubPath = "build/cluster/modules";       // When ran from source tree
          int idx = ExePath.indexOf(SubPath);
          if (idx < 0) {
              SubPath = "ops_idl_builder_nb/modules";    // When ran from deploy tree/install directory
              idx = ExePath.indexOf(SubPath);
              if (idx < 0) {
                  SubPath = "";
                  ExePath = "";
              } else {
                  ExePath = ExePath.substring(0, idx);
              }
          } else {
              ExePath = ExePath.substring(0, idx);
          }
          ExePath = ExePath.replaceFirst("file:/", "");
        }
        catch (Exception e) {
            SubPath = "";
            ExePath = "";
        }
        System.out.println("Debug: ExePath: " + ExePath);
        //io.getOut().println("Debug: ExePath: " + ExePath);

        // resolve path to OPSJLib. Not required for this app but for generated java code
        String OPSJLibpath = ops.OPSObject.class.getProtectionDomain().getCodeSource().getLocation().getPath();
        try {
            String decodedPath = java.net.URLDecoder.decode(OPSJLibpath, "UTF-8");
            OPSJLibpath = decodedPath;
        } catch(java.io.UnsupportedEncodingException uee) {
            System.out.println("Failed to decode path to " + OPSJLibpath);
        }
        System.out.println("Path to OPSJLib: " + OPSJLibpath);

        // resolve path to ConfigurationLib
        String ConfigurationLibpath = configlib.Serializable.class.getProtectionDomain().getCodeSource().getLocation().getPath();
        try {
            String decodedPath = java.net.URLDecoder.decode(ConfigurationLibpath, "UTF-8");
            ConfigurationLibpath = decodedPath;
        } catch(java.io.UnsupportedEncodingException uee) {
            System.out.println("Failed to decode path to " + ConfigurationLibpath);
        }
        System.out.println("Path to configlib: " + ConfigurationLibpath);

        jarDepString += "\"" + OPSJLibpath + "\"" + File.pathSeparator;
        
        // Add OPS libs to jarDepString
        if (!SubPath.isEmpty()) {
            jarDepString += "\"" + ExePath + SubPath + "/ext/OPSJLib.jar\";";
            jarDepString += "\"" + ExePath + SubPath + "/ext/ConfigurationLib.jar\"";
        } else {
            System.out.println("ERROR: Can't find OPS files: 'OPSJLib.jar' & 'ConfigurationLib.jar'");
            //io.getOut().println("ERROR: Can't find OPS files: 'OPSJLib.jar' & 'ConfigurationLib.jar'");
        }

        String classOutputDir = "\"" + _outputDir + File.separator + "Generated" + File.separator + "tmp\"";
        //classOutputDir = classOutputDir.replace("/", "\\");
        
        String execString = "javac -cp " + jarDepString +
///                "\"" + ExePath + "build/cluster/modules/ext/OPSJLib.jar\";" +
///                "\"" + ExePath + "ops_idl_builder_nb/modules/ext/OPSJLib.jar\";" +
///                "\"" + ExePath + "build/cluster/modules/ext/ConfigurationLib.jar\";" +
///                "\"" + ExePath + "ops_idl_builder_nb/modules/ext/ConfigurationLib.jar\"" +
                " @" + "\"" + dinfoPath + "\"";

        String batFileText;
        if(isLinux) {
            batFileText = "#!/bin/sh" + endl();
            batFileText += "set -e # exit on err" + endl() + endl();
        } else {
            batFileText = ";";//"@echo off" + endl();
        }
        batFileText += "echo Building Java..." + endl();
        batFileText += "javac -version" + endl();
        if(isLinux) {
            batFileText += "mkdir -p " + classOutputDir + endl();
        } else {
            // on windows, mkdir works like posix mkdir -p IF commandline extensions are enabled
            batFileText += "mkdir " + classOutputDir + endl();
        }
        batFileText += "cd " + classOutputDir + endl();
        batFileText += execString + endl();

        //String projDirUp = projectDirectory.substring(0, projectDirectory.lastIndexOf("/Generated"));
        //String projectName = projectDirectory.substring(projDirUp.lastIndexOf("/"), projDirUp.length());


        jarPackString = "jar cfm \"" + _outputDir + File.separator + 
            _projectName + ".jar\" \"" + manFilePath + "\" -C \"" +
            _outputDir + "Java" + "\" . ";
        batFileText += jarPackString + endl();
        batFileText += "echo done." + endl();
        if(!isLinux) {
            batFileText += "pause" + endl();
            batFileText += "exit" + endl();
        }
//        batFileText += "del \"" + projectDirectory + "manifest_adds.ops_tmp\"\n";

        //Process p = Runtime.getRuntime().exec(jarPackString);
        //projDirUp = projDirUp.replace('/', '\\');

        if(isLinux) {
            String script = _outputDir + File.separator + "java_build_script.sh";
            java.io.Writer output = new java.io.BufferedWriter(new java.io.FileWriter(script));
            output.write(batFileText);
            output.close();
            //createAndWriteFile(_outputDir + File.separator + "java_build_script.sh", batFileText);

            Runtime rTime = Runtime.getRuntime();
            // make script executable
            rTime.exec("chmod u+x " + script);
            
            System.out.println("Info: cmd /c start /D \"" + _outputDir + "\" java_build_script.bat");

            Process process = rTime.exec("sh " + script);

            process.waitFor();
        } else {
        createAndWriteFile(_outputDir + File.separator + "java_build_script.bat", batFileText);
        //System.out.println("HELM removed: " + projDirUp + "/java_build_script.bat");
        //FileHelper.createAndWriteFile(projDirUp + "/java_build_script.bat", batFileText);

        //Process p = Runtime.getRuntime().exec("java_build_script.bat");
        //Thread.sleep((1000));
        //p.waitFor();
        Runtime rTime = Runtime.getRuntime();
        System.out.println("Info: cmd /c start /D \"" + _outputDir + "\" java_build_script.bat");
        //io.getOut().println("Info: cmd /c start /D \"" + projDirUp + "\" java_build_script.bat");
        Process process = rTime.exec("cmd /c start /D \"" + _outputDir + "\" java_build_script.bat");

//            InputStream p_in = process.getInputStream();
//            OutputStream p_out = process.getOutputStream();
//            InputStream p_err = process.getErrorStream();
        process.waitFor();
//            p_in.close();
//            p_out.close();
//            p_err.close();
        }

        // --------------------------------------------------------------------
        // Test code
        // Write javac version number in output window
        try {
          ProcessBuilder pb = new ProcessBuilder("javac", "-version");
          pb.redirectErrorStream(true);
          Process p = pb.start();
          InputStream inp = p.getInputStream();

          int c;
          while ((c = inp.read()) != -1) {
              //io.getOut().write(c);
              System.out.write(c);
          }
        }
        catch (IOException e) {
          System.out.println("Error: " + e.getMessage());
          //io.getOut().println("Error: " + e.getMessage());
        }
        // --------------------------------------------------------------------

    }

    public void appendFileToBuild(List<String> file)
    {
        for (String string : file)
        {
            createdFiles += "\"" + string + "\"\n";
        }
        
    }
}
