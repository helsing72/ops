# Build Instructions #

## Checkout OPS ##
Use a Git client of your choice to checkout OPS to a directory of your choice e.g. "C:\OPS" (OPS\_DIR). You can choose to checkout either trunk, a tag or a branch of your choice. Tags should be compilable and deployable right away. Trunk should normally build but is not sure to function properly. Refer to the latest commit comments to see if there are known issues or unfinished work in progress.

## Install Boost (only if you want to build OPS for C++) ##
Download pre-built binaries for boost or build boost from source  http://www.boost.org/users/download/.

  * Select at least the following components:
    1. Boost header files
    1. Boost DateTime
    1. Boost Filesystem
    1. Boost Regex
    1. Boost System
    1. Boost Thread

OPS has been used with Boost versions 1.38, 1.48, 1.55 and 1.60.

## Building with CMake (Windows and Linux) ##

TBD


## Build OPS Core for C++ with Visual Studio ##
Open solution/project for your version of Visual Studio (has been used with
VS2008, VS2010, VS2012, VS2013 and VS2015) in directory

> "OPS\_DIR\Cpp\Visual C++\OPSCrossPlatformCppSolution_20xx\OPSCrossPlatformCppLib_20xx.sln"

Check that the include and library references to boost corresponds with your installation (see Visual Studio project properties).

Build the solution for all configurations (both x86 and x64):

  * "Debug" - for compiler setting Multi-threaded Debug
  * "Debug DLL" - for compiler setting Multi-threaded Debug DLL
  * "Release" - for compiler setting Multi-threaded Release
  * "Release DLL" - for compiler setting Multi-threaded Release DLL

Verify they all compile alright (you will have some warnings).

## Build OPS Core for C# with Visual Studio ##

TBD


## Build OPS Core for Java with Netbeans 6.7 ##

Open from Netbeans
> "Java\OPSJLib" and "Libs\ConfigurationLib".
First build ConfigurationLib and then OPSJLib. Both should compile without errors.


## Build opsc (command-line OPS IDL compiler) on Windows ##

TBD


## Build OPS IDL Builder with Netbeans 6.7 ##

Open from Netbeans "Tools\OPS IDL Builder NB", make sure you check Open Required Projects and Open as main project. Right click the main project and select "Build All". This will build several projects and output quite a lot of text in the output window. Make sure you scroll to the bottom of the output window and verify it says "BUILD SUCCESSFUL". Try the OPS IDL Builder by choosing "Run" on the main project (OPS IDL Builder NB).

## Deploy Binaries ##

TBD

Under OPS\_DIR\DeployScripts, there are three runnable bat files that package Java, C++ and Tools binaries. The deploy script are simple and you can edit them to fit your choices as you like.
