# Prerequisites to Build OPS #

## Git ##
Make sure you have a git client installed.

## Java ##
Make sure you have a Java SE Development Kit (JDK, version 8 or higher) installed on
your system. The tools you use to create systems with OPS are all running in Java and
use the Java compiler to generate pre-compiled code to use for debugging. If you know
you will only use OPS for C++ and C# and do not need any debugging capabilities, it may
be sufficient to only have a Java runtime environment installed. It is strongly
recommended to install a JDK though.

If you do not have a JDK installed on your system follow these steps:
  1. Download the latest JDK from oracle.com, (Tip! Select a bundle with Netbeans and you are ready to open the OPS sample Netbeans Java projects!)       http://www.oracle.com/technetwork/java/index.html
  1. Install it, you will need administrator rights on your system.
  1. When installed, add the newly created directory
 > on Windows e.g. "C:\Program Files\Java\jdk1.8.0_05\bin"

 to your user environment variable called path, you can do this without being an administrator of your system.

## Boost (required for OPS C++ Core) ##
Make sure you have boost installed, otherwise download pre-built binaries for boost or build boost from source http://www.boost.org/users/download/.

  * Select at least the following components:
    * Boost header files
    * Boost DateTime
    * Boost Filesystem
    * Boost Regex
    * Boost System
    * Boost Thread

OPS has been used with Boost versions 1.38, 1.48, 1.53, 1.55 and 1.60.

## CMake ##
Version 3.1.0 or later is required to build OPS with CMake. Make sure the cmake command is in your path. CMake can be downloaded at https://cmake.org/.

OPS has been used with CMake version 3.5.0 and later.

## Visual Studio (Windows) ##
OPS has been used with VS2008, VS2010, VS2012, VS2013, VS2015 and VS2017.

## Google Test (required for unit tests) ##
Google Test can be downloaded at https://github.com/google/googletest.

## JavaCC (required for rebuilding the IDL parser) ##
The Java Compiler Compiler is used to generate the IDL parser for the OPS IDL language.
JavaCC can be downloaded from http://java.net/projects/javacc.

OPS uses JavaCC version 6.0.

## Python ##
Experimental support exist for Python 2.7.

## Delphi ##
OPS has been used with Delphi XE2 and Delphi 10.1 Berlin.

## Ada ##
The Ada implementation requires Ada 2012 support.
OPS has been used with GNAT GPS 2016.

## gcovr ##
TBD

## cppcheck ##
TBD
