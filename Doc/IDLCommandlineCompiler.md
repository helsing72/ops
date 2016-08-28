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
    for generate: cpp, csharp, delphi, java, python, debug
    for build:    csharp, java

```

Examples of usage:

To compile IDL's for the TestAll example, run the following command:
```
  D:\OPS\OPS4> deploy\lib\ops\opsc.bat -P Examples\OPSIdls\TestAll
```
This creates a 'Generated' directory in the TestAll directory with all generated files.
