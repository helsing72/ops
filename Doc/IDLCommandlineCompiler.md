[Examples](SimpleCpp.md) [BuildInstructions](BuildInstructions.md)

---

# OPS IDL Compiler #
Learn more about the OPS IDL language [here](IDLLanguage.md).

To compile IDL files into target languages, OPS comes with a command line tool called ***opsc***.
The following shows the available command arguments:

```
D:\OPS\OPS4>deploy\lib\ops\opsc.bat -h

  usage: opsc [options] [idlfiles ...]

  OPTIONS
    -o <dir>          set output directory
    -t <dir>          set template directory (overrides built-in templates)
    -p <projname>     set project name
    -P <IDL proj dir> use as project directory with pre-defined subdirectories
    -h | --help       show this help
    -d                verbose output
    -dump             print all parsed objects
    -pp               name an ops IDL project.properties file
    -parse            only parse, don't generate
    -printProps       print system props
```

Examples of usage:
TBD
