@echo Building needed libraries ...
@pushd Libs\ConfigurationLib
@call Build_Lib.bat
@popd
@pushd Java\OPSJLib
@call Build_Lib.bat
@popd
@pushd Tools\OPSCompilerLib
@call Build_Lib.bat
@popd
@pushd Tools\IDLParser
@call Build_Lib.bat
@popd
@echo Building IDL Command-Line Compiler ...
@pushd Tools\opsc
@call Build_opsc.bat
@popd
