[Examples](SimpleCpp.md) [BuildInstructions](BuildInstructions.md)

---

# OPS IDL Compiler #
Learn more about the OPS IDL language [here](IDLLanguage.md).

To compile IDL files into target languages, OPS comes with a command line tool called ***opsc***.
The following shows the available command arguments:

```
D:\OPS\OPS4>deploy\lib\ops\opsc.bat -h

Invoking script at: D:\OPS\OPS4\deploy\lib\ops\
Current working directory: D:\OPS\OPS4

  opsc -P <IDL_proj_dir> [options]
    or
  opsc [options] idlfiles...

  OPTIONS
    -? | -h | --help  show this help
    -b <feature>      build given feature
    -B <feature>      don't build given feature
    -d                verbose output
    -dump             print all parsed objects
    -fac              only generate factories for given features
    -g <feature>      generate given feature
    -G <feature>      don't generate given feature
    -o <dir>          set output directory
    -P <IDL_proj_dir> use as project directory with pre-defined subdirectories
    -p <projname>     set project name
    -parse            only parse, don't generate
    -pp <file>        name an ops IDL project.properties file
    -printProps       print system props
    -t <dir>          set template directory (overrides built-in templates)

  FEATURE
    for generate: ALL, ada, cpp, csharp, delphi, java, json, python, debug
    for build:    ALL, csharp, java    

```

Examples of usage:

To compile IDL's for the TestAll example, run the following command:
```
  D:\OPS\OPS4> deploy\lib\ops\opsc.bat -P Examples\OPSIdls\TestAll
```
This creates a 'Generated' directory in the TestAll directory with all generated files.

NOTE: For ***opsc*** to compile C# code with the 'correct' C# compiler, you may need to set the
environment symbol **OPS_CSC_PATH** to the directory where the wanted compiler binary exist.

Example:

```
OPS_CSC_PATH=C:\Windows\Microsoft.NET\Framework\v4.0.30319  
```
