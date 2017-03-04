# Delphi OPS Implementation #

The Delphi implementation is now in pair with the C++ implementation regarding functionality.

The implementation uses generics which arrived in Delphi 2009 and namespaces that requires Delphi XE2.
The implementation has only been tested with **Delphi XE2** and **Delphi 10.1 Berlin** Starter Edition, but should work with any version from Delphi XE2 and forward.

There are two ways to use OPS in our program, either:

* Add all source files in *Common* and *Source* directories to your project or

* Compile OPS into a package using the Delphi project file *OPS4.dproj* and then add **OPS4** to your projects options under *Project > Options > Packages > Runtime Packages* (only tested on **Delphi 10.1 Berlin**).
