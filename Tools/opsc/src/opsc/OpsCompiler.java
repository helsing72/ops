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
import java.util.Vector;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author helm
 */
public class OpsCompiler
{
    List<String> _listInputFiles = new ArrayList<String>();
    String _strProjectName = "";
    boolean _bOnlyParse = false;
    /** An instance of ProjectProperties is used to hold defaults 
     *  as well is modifications to defaults either read from the
     *  command-line or project files or what have you */
    private ProjectProperties _props = new ProjectProperties();

    /** The IDL parser creates IDLClass instances from idl files */
    private opsc.IDLParser _parser;

    /** A verbosity flag. Currently supports 0 or not 0 */
    int _verbose = 0;

    /** The java generator instance */
    protected opsc.JavaCompiler _javaCompiler;

    public OpsCompiler() {
        // create a parser to generate IDLClasses for us
        _parser = new opsc.IDLParser();
    }

    public static void usage() {
        System.out.println("opsc [options] idlfiles...");
        System.out.println("");
        System.out.println("OPTIONS");
        System.out.println("  -o <dir>      set output directory");
        System.out.println("  -t <dir>      set template directory");
        System.out.println("  -p <projname> set project name");
        System.out.println("  -h | --help   show this help");
        System.out.println("  -d            verbose output");
        System.out.println("  -pp           name a netbeans project.properties file");
        System.out.println("  -parse        only parse, don't generate");
        System.out.println("  -printProps   print system props");
        System.out.println("");
        System.exit(1);
    }

    protected boolean parseCommandLineArgs(String args[]) {
        System.out.println("Parse arguments...");

        // first find out if we have a project property spec
        for(int i = 0 ; i < args.length ; i++) {
            if(args[i].equals("-pp") && i < args.length) {
                i++;
                System.out.println("Parse projectproperties in " + args[i]);

                // read file
                try {

                java.io.FileInputStream fis = new java.io.FileInputStream(args[i]);
                configlib.XMLArchiverIn xml = new configlib.XMLArchiverIn(fis, "properties");
                _props.serialize(xml);

                } catch (java.lang.NullPointerException npe) {
                    System.out.println("configlib is buggy!");
                } catch (java.io.FileNotFoundException fnfe) {
                    System.out.println("no such file: " + args[i]);
                } catch(java.io.IOException ioe) {
                    System.out.println(args[i] + " can't be read from");
                } catch (configlib.exception.FormatException fe) {
                    System.out.println(args[i] + " is badly formatted");
                }
            }
        }

        // parse arguments
        for(int i = 0 ; i < args.length ; i++) {
            if(args[i].equals("-o") && i < args.length) {
                i++;
                _props.setProperty(new Property("outputPath", args[i]));
                System.out.println("Output path set to " + _props.getPropertyValue("outputPath", null));
            } else if(args[i].equals("-t") && (i < args.length)) {
                i++;
                _props.setProperty(new Property("templatePath", args[i]));
                System.out.println("Template path set to " + _props.getPropertyValue("templatePath", null));
            } else if(args[i].equals("-p") && (i < args.length)) {
                i++;
                _strProjectName = args[i];
                System.out.println("Project name set to " + _strProjectName);
            } else if(args[i].equals("-d")) {
                _verbose = 1;
            } else if(args[i].equals("-dd")) {
                _verbose = 2;
            } else if(args[i].equals("-parse")) {
                _bOnlyParse = true;
            } else if(args[i].equals("-h") || args[i].equals("--help")) {
                usage();
                return true;
            } else if(args[i].equals("-printProps")) {
                System.getProperties().list(System.out);
                _props.list(System.out);
                usage();
                return true;
            } else if(args[i].equals("-pp")) {
                // ignore -pp here
                i++;
            } else {
                // not a known option - regard as input file
                _listInputFiles.add(args[i]);
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
            System.out.println(inputfile.getName() + " doesn't exist");
            return false;
        } catch(java.io.IOException ioe) {
            System.out.println(inputfile.getName() + " can't be read from");
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
            _javaCompiler.buildAndJar(_strProjectName);
        } catch(java.io.IOException ioe) {
            System.out.println("Failed to buildAndJar " + _strProjectName);
        } catch(java.lang.InterruptedException inte) {
            System.out.println("Failed to buildAndJar (Interrupted) " + _strProjectName);
        }
        
        return true;
    }

    protected boolean compileCs() {
        // create the compiler and set parameters
        opsc.Compiler compiler = new opsc.CSharpCompiler(_strProjectName);
        Property propTemplatePath = _props.getProperty("templatePath");
        if(propTemplatePath != null)
            compiler.setTemplateDir(propTemplatePath.value);
        Property propOutPath = _props.getProperty("outputPath");
        if(propOutPath != null)
            compiler.setOutputDir(propOutPath.value + File.separator + "CSharp");

        compiler.compileDataClasses(_parser._idlClasses, "baba");
        compiler.compileTypeSupport();

        return true;
    }
    
    public static void main(String args[]) {

        // instantiate this class
        OpsCompiler opsc = new OpsCompiler();
        if(!opsc.parseCommandLineArgs(args)) {
            System.out.println("Bad args in command line");
            // return with an exit code
            System.exit( -1 );
        }
        
        // Debug output
        if(opsc._verbose > 0) {
            System.out.println("command line args ("+ (args.length-1) + ")");
            for(int i = 0 ; i < args.length ; i++) {
                System.out.println("arg" + i + ": " + args[i] );
            }
        }
        
        if(opsc._listInputFiles.isEmpty()) {
            System.out.println("No input files.");
            return;
        }


        // iterate over all idl files on the cmd line
        for(String input : opsc._listInputFiles) {
            // Debug output
            if(opsc._verbose > 0) {
                System.out.println("input: " + input);
            }
            
            if(input.endsWith(".idl")) {
                java.io.File inputfile = new java.io.File(input);
                opsc.parseFile(inputfile);

            } else if(input.endsWith(".prj")) {
                System.out.println("We dont support prj files");
            } else {
                System.out.println(input + " unknown input type");
            }
        }

        // generate c++ if requested
        if(opsc._props.generateCpp)
            opsc.compileCpp();

        // generate java if so requested
        if(opsc._props.generateJava) {
            // if compile is successful and user opted to build java
            if(opsc.compileJava() && opsc._props.buildJava) {
                opsc.buildJava();
            }
        }

        // generate cs if so requested
        if(opsc._props.generateCS)
            opsc.compileCs();
    }
};
