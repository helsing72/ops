[Examples](SimpleCpp.md) [BuildInstructions](BuildInstructions.md)

---

# Building Examples #
Under OPS_DIR/Examples, you will find example IDL's in the directory OPSIdls.
One of these examples is called **TestAll**. A corresponding C++ implementation
is called **TestAllC++Test**.

## Build TestAll Example IDLs ##
This requires that you have built the *opsc* command-line compiler.

On Windows do
> \> path-to-opsc\opsc.bat -P OPSIdls\TestAll

On Linux do
> $ path-to-opsc/opsc.sh -P OPSIdls/TestAll

This creates a 'Generated' directory in the TestAll directory with all generated files.

## Build TestAll C++ Example ##
Open **TestAllC++Test.sln** under **TestAllC++Test**, check the boost reference to the include and library directories so they point to your boost installation and then build and run to see that your OPS installation works correctly for C++.
