# OPS Listener #
To listen to OPS traffic, OPS comes with a command line tool called ***OPSListener***.
The following shows the available command arguments:

```
D:\OPS\OPS4>deploy\bin\OPSListener -?

OPSListener Version 2016-05-14

  This program can subscribe to any OPS topic and it is possible to choose what
  information to present and in which order.
  This can be used to test if / verify that topics are published.

Usage:
  OPSListener [-v] [-?] [-c ops_cfg_file [-c ops_cfg_file [...]]]
              [-t] [-pA | -p<option_chars>]
              [-a arg_file [-a arg_file [...]]]
              [-IA | -I domain [-I domain [...]] [-O]]
              [-SA | -S domain [-S domain [...]]]
              [-D default_domain] Topic [Topic ...]

    -?                 Shows a short description
    -a arg_file        File with command line arguments
    -c ops_config_file Specifies an OPS configuration file to use
                       If none given, the default 'ops_config.xml' is used
    -D default_domain  Default domain name to use for topics given without domain name
                       If none given, the default 'SDSDomain' is used
                       A new default can be given between topics
    -I domain          Subscribe to Participant Info Data from given domain
    -IA                Subscribe to Participant Info Data from all domains in given configuration files
    -O                 if -I or -IA given, only show arriving and timed out participants
    -p<option_chars>   Defines for received messages, which fields to print and in which order
                 n       Publisher Name
                 i       Publication Id
                 T       Topic Name
                 y       Type
                 s       Sparebytes Size
                 k       Key
                 S       Source IP::Port
    -pA                Short for all option chars in the program default order
    -S domain          Subscribe to all topics in given domain
    -SA                Subscribe to all topics in all domains in given configuration files
    -t                 Print receive time for each message
    -v                 Verbose output during parsing of command line arguments
```

Examples of usage:
TBD

The source for the tool is in the OPS _Tools_ folder and the built binary in the _deploy/bin_ folder.
