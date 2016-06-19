@rem find out where this script is
@set SCRIPT_PATH=%~dp0

@pushd %~dp0

IF NOT EXIST Tools mkdir Tools

copy "..\Tools\OPS IDL Builder NB\dist\ops_idl_builder_nb.zip" "Tools"
rename "Tools\ops_idl_builder_nb.zip" OPS_IDL_Compiler.jar
cd Tools
rmdir /S /Q "OPS IDL Builder"
jar xf OPS_IDL_Compiler.jar
del OPS_IDL_Compiler.jar
rename ops_idl_builder_nb "OPS IDL Builder"
cd ..
mkdir "Tools\OPS IDL Builder\lib"
copy "..\Java\OPSJLib\dist\OPSJLib.jar" "Tools\OPS IDL Builder\lib"
copy "..\Libs\ConfigurationLib\dist\ConfigurationLib.jar" "Tools\OPS IDL Builder\lib"

@rem mkdir "Tools\OPS Debugger"
@rem mkdir "Tools\OPS Debugger\lib"
@rem copy "..\ToolS\OPSDebugger2\dist\*.*" "Tools\OPS Debugger"
@rem copy "..\ToolS\OPSDebugger2\dist\lib\*.*" "Tools\OPS Debugger\lib"

@rem jar cvf OPS_Tools.jar deploy\OPS\Tools
@rem rename OPS_Tools.jar OPS_Tools.zip

pause

@popd
