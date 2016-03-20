@echo Building needed libraries ...
@call Libs\ConfigurationLib\Build_Lib.bat
@call Java\OPSJLib\Build_Lib.bat
@call Tools\OPSCompilerLib\Build_Lib.bat
@call Tools\IDLParser\Build_Lib.bat
@call Tools\NBOPSIDLSupport\Build_Lib.bat

@echo Building IDL Command-Line Compiler ...
@call Tools\opsc\Build_opsc.bat
