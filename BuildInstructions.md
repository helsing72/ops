# Build Instructions #

## Checkout OPS ##
Use an SVN client of your choice (e.g. [Tortoise](http://tortoisesvn.tigris.org)) to    checkout OPS to a directory of your choice e.g. "C:\OPS" (OPS\_DIR), see Source tab. You can choose to checkout either trunk, a tag or a branch of your choice. Tags should be compilable and deployable right away. Trunk should normally build but is not sure to function properly. Refer to the latest commit comments to see if there are known issues or unfinished work in progress.

## Install Boost (only if you want to build OPS for C++) ##
Download pre-built binaries for boost 1.38 from http://www.boostpro.com/download
  * Under "Select Default Variants" in the installer choose the build versions of boost you need (depends on how you want to build and link OPS). If you dont know, select all.
  * Under "Choose Components", select at least the following (make sure you only select components for the compiler versions you need):
    1. Boost header files
    1. Boost DateTime
    1. Boost Filesystem
    1. Boost Regex
    1. Boost System
    1. Boost Thread
Hit next, and choose Installation Directory to be for example "C:\Program Files\boost\boost\_1\_38" and start installation.

## Build OPS Core for C++ with Visual Studio ##
Open

> "OPS\_DIR\C++\Visual C++\OPSCrossPlatformCppSolution\OPSCrossPlatformCppSolution.sln"

with Visual Studio. If you didn't install boost to "C:\Program Files\boost\boost\_1\_38" you need to repoint your include directories in the Visual Studio Project (see project properties-linker).

Build the solution for all configurations:

  * "Debug" - for compiler setting Multi-threaded Debug
  * "Debug DLL" - for compiler setting Multi-threaded Debug DLL
  * "Release" - for compiler setting Multi-threaded Release
  * "Release DLL" - for compiler setting Multi-threaded Release DLL

verify they all compile alright (you will have some warnings).

## Build OPS Core for Java with Netbeans 6.7 ##

Open from Netbeans
> "Java\OPSJLib" and "Libs\ConfigurationLib".
First build ConfigurationLib and then OPSJLib. Both should compile without errors.

## Build OPS IDL Builder with Netbeans 6.7 ##

Open from Netbeans "Tools\OPS IDL Builder NB", make sure you check Open Required Projects and Open as main project. Right click the main project and select "Build All". This will build several projects and output quite a lot of text in the output window. Make sure you scroll to the bottom of the output window and verify it says "BUILD SUCCESSFUL". Try the OPS IDL Builder by choosing "Run" on the main project (OPS IDL Builder NB).

## Deploy Binaries ##
Once you have built all the projects, you can run deploy scripts to get them packaged up like the binaries you can download from the Source tab of this page.

Under OPS\_DIR\DeployScripts, there are three runnable bat files that package Java, C++ and Tools binaries. The deploy script are simple and you an edit them to fit your choices as you like. Just make sure you don't commit changes to them that include absolute directories or personal settings.

To deploy the examples, make a svn export to your deploy directory.