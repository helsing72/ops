
mkdir Tools

copy "..\Tools\OPS IDL Builder NB\dist\ops_idl_builder_nb.zip" "Tools"
rename "Tools\ops_idl_builder_nb.zip" OPS_IDL_Compiler.jar
cd Tools
jar xf OPS_IDL_Compiler.jar
del OPS_IDL_Compiler.jar
rename ops_idl_builder_nb "OPS IDL Builder"
cd ..
mkdir "Tools\OPS IDL Builder\lib"
copy "..\Java\OPSJLib\dist\OPSJLib.jar" "Tools\OPS IDL Builder\lib"
copy "..\Libs\ConfigurationLib\dist\ConfigurationLib.jar" "Tools\OPS IDL Builder\lib"

mkdir "Tools\OPS Debugger"
mkdir "Tools\OPS Debugger\lib"
copy "..\ToolS\OPSDebugger2\dist\*.*" "Tools\OPS Debugger"
copy "..\ToolS\OPSDebugger2\dist\lib\*.*" "Tools\OPS Debugger\lib"

rem jar cvf OPS_Tools.jar deploy\OPS\Tools
rem rename OPS_Tools.jar OPS_Tools.zip
