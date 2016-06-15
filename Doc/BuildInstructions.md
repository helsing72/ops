# Build Instructions #

## Prerequisites ##
See [Prerequisites](Prerequisites.md) for the required tools to build OPS.

## Checkout OPS ##
Use a Git client of your choice to clone the OPS repository to a directory of your choice, on windows e.g. "C:\\OPS" (OPS_DIR). You can then choose to checkout either trunk, a tag or a branch of your choice. Tags should be compilable and deployable right away. Trunk should normally build but is not sure to function properly. Refer to the latest commit comments to see if there are known issues or unfinished work in progress.

## Building with CMake ##
#### Linux ####
In directory "OPS_DIR" type 'make'.

This will build both 'debug' and 'optimized' builds and put the intermediate result in subdirectories 'build.debug' and 'build.opt'.

#### Windows ####
Building with CMake on Windows currently requires Visual Studio.

Start a *MSBuild* command prompt, change to "OPS_DIR" and run the *run-cmake.bat* file.

This will build both 'debug' and 'optimized' builds and put the intermediate result in subdirectories 'build.debug' and 'build.opt'.

#### CMake result ####
The install result will be in the subdirectory 'deploy' with the following structure:

> deploy
>  - bin
>  - include
>  - lib
>  - share

## Building without CMake ##
See [Building without CMake](BuildingWithoutCMake.md).

## Building OPS IDL Builder with Netbeans ##
Open from Netbeans "OPS_DIR\Tools\OPS IDL Builder NB", make sure you check Open Required Projects and Open as main project. Right click the main project and select "Build All". This will build several projects and output quite a lot of text in the output window. Make sure you scroll to the bottom of the output window and verify it says "BUILD SUCCESSFUL". Try the OPS IDL Builder by choosing "Run" on the main project (OPS IDL Builder NB).

Has been used with Netbeans 6.7, 6.9 and 8.0.

## Building Examples ##
See [Building Examples](BuildingExamples.md).

## Deploy Binaries ##
TBD

Under OPS_DIR\Scripts, there are three runnable Deploy bat-files that package Java, C++ and Tools binaries. The deploy scripts are simple and you can edit them to fit your choices as you like.
