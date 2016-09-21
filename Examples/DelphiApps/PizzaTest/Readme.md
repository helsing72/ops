# Delphi PizzaTest Example #

PizzaTest is an example program using the OPS_DIR/Examples/OPSIDLs/PizzaProject IDL-files. There exist corresponding examples for the other supported languages, so they can be used together.

## Opening the PizzaTest project ##

Open the *PizzaTest.dpr* or *PizzaTest.dproj* project files.

The project references all OPS Delphi-files, the generated PizzaProject files and the actual test application. To be able to compile, you need to have generated the Delphi code from the PizzaProject IDL-files, see below.

## Generating Delphi code from IDL-files ##

To be able to generate the Delphi code, it requires the **opsc** compiler to be built, which in turn requires *Java* to be installed, see [Build  Instructions](../../../Doc/BuildInstructions.md). This places the **opsc** compiler in either *OPS_DIR/Tools/opsc* or *OPS_DIR/deploy/lib/ops* depending on how the compiler is built.

To compile IDL's for the PizzaTest example, run the following command:
```
  D:\OPS\OPS4> deploy\lib\ops\opsc.bat -P Examples\OPSIdls\PizzaProject -g delphi
```
This creates a 'Generated' directory in the PizzaProject directory with all generated files, including Delphi-files.

See the [OPS User Guide](../../../Doc/UserGuide.md) for more information.
