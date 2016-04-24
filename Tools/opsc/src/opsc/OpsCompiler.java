/** -*-java-*- Copyright (C) 2014 Saab Dynamics AB
 ***************************************************************************
 *  @file   opsc/OpsCompiler.java
 *  @author Mattias Helsing <mattias.helsing@saabgroup.com>
 *
 * An attempt to create a command line application for generating
 * Cpp files from OPS IDL files. Should be extended to generate C#
 * and java classes too.
 *
 ***************************************************************************
 */

package opsc;

import java.io.FileOutputStream;
import java.util.ArrayList;
import java.util.List;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.io.FileFilter;
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.net.URL;

import parsing.IDLClass;
import parsing.IDLField;

/**
 *
 * @author helm
 */
public class OpsCompiler
{
    List<String> _listInputFiles = new ArrayList<String>();
    String _strProjectName = "";
    String _strProjectDir = "";
    boolean _bOnlyParse = false;

    /** An instance of ProjectProperties is used to hold defaults
     *  as well is modifications to defaults either read from the
     *  command-line or project files or what have you */
    private ProjectProperties _props = new ProjectProperties();

    /** The IDL parser creates IDLClass instances from idl files */
    private opsc.IDLParser _parser;

    /** A verbosity flag. Currently supports 0 or not 0 */
    int _verbose = 0;
    boolean _dumpFlag = false;

    /** The java generator instance */
    protected opsc.JavaCompiler _javaCompiler;
    protected opsc.CSharpCompiler _CSharpCompiler;

    public OpsCompiler() {
        // create a parser to generate IDLClasses for us
        _parser = new opsc.IDLParser();
    }

    public static void usage() {
        System.out.println("opsc [options] idlfiles...");
        System.out.println("");
        System.out.println("OPTIONS");
        System.out.println("  -o <dir>          set output directory");
        System.out.println("  -t <dir>          set template directory (overrides built-in templates)");
        System.out.println("  -p <projname>     set project name");
        System.out.println("  -P <IDL proj dir> use as project directory with pre-defined subdirectories");
        System.out.println("  -h | --help       show this help");
        System.out.println("  -d                verbose output");
        System.out.println("  -dump             print all parsed objects");
        System.out.println("  -pp               name an ops IDL project.properties file");
        System.out.println("  -parse            only parse, don't generate");
        System.out.println("  -printProps       print system props");
        System.out.println("");
        System.exit(1);
    }

    /**
     * A class that implements the Java FileFilter interface.
     */
    public class IdlFileFilter implements FileFilter
    {
      public boolean accept(File file)
      {
        if (file.isDirectory()) return true;
        return file.getName().toLowerCase().endsWith("idl");
      }
    }

    // Add recursively all idl-files starting with directory 'src'
    protected void addFilesFromDirectory(File dir, Vector<String> extraArgs)
    {
      File[] files = dir.listFiles(new IdlFileFilter());
      for (File f : files) {
        if (f.isDirectory()) {
          if (f.getName().equals(".")) continue;
          if (f.getName().equals("..")) continue;
          addFilesFromDirectory(f, extraArgs);
        } else {
          if (_verbose > 0) System.out.println("Debug: Adding arg: " + f.getAbsolutePath());
          extraArgs.add(f.getAbsolutePath());
        }
      }
    }

    protected boolean parseCommandLineArgs(String args[])
    {
        //System.out.println("Info: Parse arguments...");

        Vector<String> extraArgs = new Vector<String>();
        boolean projDirGiven = false;

        // first find out if we have a project directory given
        for (int i = 0 ; i < args.length ; i++) {
          if (args[i].equals("-P") && (i < args.length)) {
            if (projDirGiven) {
              System.out.println("Error: only one '-P' argument is allowed");
              return false;
            }
            projDirGiven = true;
            i++;
            // System.out.println("Project Directory: " + args[i]);
            String path = args[i];

            // Check that folders 'opsproject' and 'src' exist
            // and that 'opsproject' contains 'project.properties'
            File dir = new File(path);
            if (!dir.isDirectory()) {
              System.out.println("Error: argument is not a directory: " + path);
              return false;
            }
            try {
              path = dir.getCanonicalPath();
            } catch (java.io.IOException ioe) {
            }
            _strProjectDir = path;
            //System.out.println(">>>>: _strProjectDir= " + _strProjectDir);

            File projDir = new File(path + File.separator + "opsproject");
            File srcDir = new File(path + File.separator + "src");
            if ( (!projDir.isDirectory()) || (!srcDir.isDirectory()) ) {
              System.out.println("Error: missing 'opsproject' or 'src' directories in: " + path);
              return false;
            }
            File projProp = new File(path + File.separator + "opsproject" + File.separator + "project.properties");
            if (!projProp.exists()) {
              System.out.println("Error: missing file 'project.properties' in directory 'opsproject' in: " + path);
              return false;
            }

            // if so, extract projectname from directory name,
            try {
              File proj = dir.getCanonicalFile();
              extraArgs.add("-p");
              extraArgs.add(proj.getName());
            } catch (java.io.IOException ioe) {
            }

            // and set outputPath to 'Generated',
            extraArgs.add("-o");
            extraArgs.add(path + File.separator + "Generated");

            // and the project properties,
            extraArgs.add("-pp");
            extraArgs.add(path + File.separator + "opsproject" + File.separator + "project.properties");

            // and recursively add all idl-files starting with directory 'src'.
            addFilesFromDirectory(srcDir, extraArgs);

          } else {
            // move arguments to extraArgs vector
            extraArgs.add(args[i]);
          }
        }

        // then find out if we have a project property spec
        for(int i = 0 ; i < extraArgs.size() ; i++) {
          String arg = extraArgs.elementAt(i);
          if (arg.equals("-pp") && i < extraArgs.size()) {
            i++;
            arg = extraArgs.elementAt(i);
            System.out.println("Info: Parsing project properties in " + arg);

            // read file
            try {
              java.io.FileInputStream fis = new java.io.FileInputStream(arg);
              configlib.XMLArchiverIn xml = new configlib.XMLArchiverIn(fis, "properties");
              xml.add(ProjectProperties.getSerializableFactory());

              _props.serialize(xml);

            } catch (java.lang.NullPointerException npe) {
              System.out.println("configlib is buggy!");
            } catch (java.io.FileNotFoundException fnfe) {
              System.out.println("no such file: " + arg);
            } catch(java.io.IOException ioe) {
              System.out.println(arg + " can't be read from");
            } catch (configlib.exception.FormatException fe) {
              System.out.println(arg + " is badly formatted");
            }
          }
        }

        // parse arguments
        for(int i = 0 ; i < extraArgs.size() ; i++) {
            String arg = extraArgs.elementAt(i);
            if(arg.equals("-o") && i < extraArgs.size()) {
                i++;
                arg = extraArgs.elementAt(i);
                _props.setProperty(new Property("outputPath", arg));
                System.out.println("Info: Output path set to " + _props.getPropertyValue("outputPath", null));
            } else if(arg.equals("-t") && (i < extraArgs.size())) {
                i++;
                arg = extraArgs.elementAt(i);
                _props.setProperty(new Property("templatePath", arg));
                System.out.println("Info: Template path set to " + _props.getPropertyValue("templatePath", null));
            } else if(arg.equals("-p") && (i < extraArgs.size())) {
                i++;
                arg = extraArgs.elementAt(i);
                _strProjectName = arg;
                System.out.println("Info: Project name set to " + _strProjectName);
            } else if(arg.equals("-d")) {
                _verbose = 1;
            } else if (arg.equals("-dump")) {
               _dumpFlag = true;
            } else if(arg.equals("-dd")) {
                _verbose = 2;
            } else if(arg.equals("-parse")) {
                _bOnlyParse = true;
            } else if(arg.equals("-h") || arg.equals("--help")) {
                usage();
                return true;
            } else if(arg.equals("-printProps")) {
                System.getProperties().list(System.out);
                _props.list(System.out);
                usage();
                return true;
            } else if(arg.equals("-pp")) {
                // ignore -pp here
                i++;
            } else if(arg.equals("-P")) {
                // ignore -P here
                i++;
            } else {
                // not a known option - regard as input file
                _listInputFiles.add(arg);
            }
        }
        return true;
    }

    /** Function that wraps loading a file and running it
     *  through the IDLParser. The _parser will hold all
     *  loaded classes. Nothing is stored here.
     *  @return true on success */
    protected boolean parseFile(java.io.File inputfile) {
        String fileString;

        try {
            // read file
            java.io.FileInputStream fis = new java.io.FileInputStream(inputfile);
            byte[] b = new byte[fis.available()];
            fis.read(b);
            fileString = new String(b);
        } catch(java.io.FileNotFoundException fnfe) {
            System.out.println("Error: " + inputfile.getName() + " doesn't exist");
            return false;
        } catch(java.io.IOException ioe) {
            System.out.println("Error: " + inputfile.getName() + " can't be read from");
            return false;
        }

        // parse the file
        String filename_wo_ext = inputfile.getName().substring(0, inputfile.getName().lastIndexOf("."));
        parsing.IDLClass idlclass = _parser.parse(filename_wo_ext, fileString);

        return true;
    }

    protected boolean compileCpp() {
        // create the compiler and set parameters
        opsc.CppCompiler compiler = new opsc.CppCompiler(_strProjectName);
        Property propTemplatePath = _props.getProperty("templatePath");
        if(propTemplatePath != null)
            compiler.setTemplateDir(propTemplatePath.value);
        Property propOutPath = _props.getProperty("outputPath");
        if(propOutPath != null)
            compiler.setOutputDir(propOutPath.value + File.separator + "Cpp");

        compiler.compileDataClasses(_parser._idlClasses, "baba");
        compiler.compileTypeSupport();

        return true;
    }

    protected boolean compileJava() {
        // create the compiler and set parameters
        _javaCompiler = new opsc.JavaCompiler(_strProjectName);
        _javaCompiler.setVerbose(_verbose);
        Property propTemplatePath = _props.getProperty("templatePath");
        if(propTemplatePath != null)
            _javaCompiler.setTemplateDir(propTemplatePath.value);
        Property propOutPath = _props.getProperty("outputPath");
        if(propOutPath != null)
            _javaCompiler.setOutputDir(propOutPath.value + File.separator + "Java");

        _javaCompiler.compileDataClasses(_parser._idlClasses, "baba");
        _javaCompiler.compileTypeSupport();

        return true;
    }

    protected boolean buildJava() {
        try {
            _javaCompiler.setJarDependencies(_props.javaBuildJarDependencies);
            if (_strProjectDir.equals("")) _strProjectDir = _strProjectName;
            _javaCompiler.buildAndJar(_strProjectDir);
        } catch(java.io.IOException ioe) {
            System.out.println("Error: Failed to buildAndJar " + _strProjectName);
        } catch(java.lang.InterruptedException inte) {
            System.out.println("Error: Failed to buildAndJar (Interrupted) " + _strProjectName);
        }

        return true;
    }

    protected boolean compileCs() {
        // create the compiler and set parameters
        _CSharpCompiler = new opsc.CSharpCompiler(_strProjectName);
        Property propTemplatePath = _props.getProperty("templatePath");
        if(propTemplatePath != null)
            _CSharpCompiler.setTemplateDir(propTemplatePath.value);
        Property propOutPath = _props.getProperty("outputPath");
        if(propOutPath != null)
            _CSharpCompiler.setOutputDir(propOutPath.value + File.separator + "CSharp");

        _CSharpCompiler.compileDataClasses(_parser._idlClasses, "baba");
        _CSharpCompiler.compileTypeSupport();

        return true;
    }

    protected boolean buildCs() {
        try {
            _CSharpCompiler.setDllDependencies(_props.csBuildDllDependencies);
            if (_strProjectDir.equals("")) _strProjectDir = _strProjectName;
            _CSharpCompiler.buildDll(_strProjectDir);
        } catch(java.io.IOException ioe) {
            System.out.println("Error: Failed to buildDll " + _strProjectName);
        } catch(java.lang.InterruptedException inte) {
            System.out.println("Error: Failed to buildDll (Interrupted) " + _strProjectName);
        }
        return true;
    }

    protected void dump()
    {
      System.out.println("");
      for (IDLClass idlClass : _parser._idlClasses)
      {
        System.out.println("idlClass.getPackageName()   : " + idlClass.getPackageName());
        System.out.println("idlClass.getClassName()     : " + idlClass.getClassName());
        System.out.println("idlClass.getBaseClassName() : " + idlClass.getBaseClassName());

        if (idlClass.getType() == IDLClass.ENUM_TYPE) {
          for (String str : idlClass.getEnumNames()) {
            System.out.println("  enum : " + str);
          }
          System.out.println("");
        } else {
          for (IDLField field : idlClass.getFields()) {
            System.out.println("  field.getName()     : " + field.getName());
            System.out.println("  field.getArraySize(): " + field.getArraySize());
            System.out.println("  field.getType()     : " + field.getType());
            System.out.println("  field.getComment()  : " + field.getComment());
            System.out.println("  field.getValue()    : " + field.getValue());
            System.out.println("  field.isIdlType()   : " + field.isIdlType());
            System.out.println("  field.isArray()     : " + field.isArray());
            System.out.println("  field.isStatic()    : " + field.isStatic());
            System.out.println("  field.isAbstract()  : " + field.isAbstract());
            System.out.println("");
          }
        }
      }
    }

    public static void main(String args[]) {

        // instantiate this class
        OpsCompiler opsc = new OpsCompiler();
        if(!opsc.parseCommandLineArgs(args)) {
            System.out.println("Error: Bad args in command line");
            // return with an exit code
            System.exit( -1 );
        }

        // Debug output
        if(opsc._verbose > 0) {
            System.out.println("Debug: command line args ("+ (args.length-1) + ")");
            for(int i = 0 ; i < args.length ; i++) {
                System.out.println("  arg" + i + ": " + args[i] );
            }
        }

        if(opsc._listInputFiles.isEmpty()) {
            System.out.println("Error: No input files.");
            return;
        }


        // iterate over all idl files on the cmd line
        for(String input : opsc._listInputFiles) {
            // Debug output
            if(opsc._verbose > 0) {
                System.out.println("Debug: input: " + input);
            }

            if(input.endsWith(".idl")) {
                java.io.File inputfile = new java.io.File(input);
                opsc.parseFile(inputfile);

            } else if(input.endsWith(".prj")) {
                System.out.println("Error: We dont support prj files");
            } else {
                System.out.println("Error: " + input + " unknown input type");
            }
        }

        if (opsc._dumpFlag) opsc.dump();

        ///TODO Check that all types, which are not prefixed with a 'name.',
        /// are core types or defined with the idl's we parsed.

        // Quit if we only should parse
        if(opsc._bOnlyParse) return;

        // generate c++ if requested
        if(opsc._props.generateCpp) {
            opsc.compileCpp();
        }

        // generate java if so requested
        if(opsc._props.generateJava) {
            // if compile is successful and user opted to build java
            if(opsc.compileJava() && opsc._props.buildJava) {
                opsc.buildJava();
            }
        }

        // generate cs if so requested
        if(opsc._props.generateCS) {
            // if compile is successful and user opted to build C#
            if(opsc.compileCs() && opsc._props.buildCS) {
                opsc.buildCs();
            }
        }
    }
};
