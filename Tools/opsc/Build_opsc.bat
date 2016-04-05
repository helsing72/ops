@pushd %~dp0
@echo Building opsc ...
@javac -version
@IF NOT EXIST "build/classes" (
	mkdir "build/classes"
)
@IF NOT EXIST dist (
	mkdir dist
)

@copy /Y "..\..\Libs\ConfigurationLib\dist\ConfigurationLib.jar" dist
@copy /Y "..\..\Java\OPSJLib\dist\OPSJLib.jar" dist
@copy /Y "..\OPSCompilerLib\dist\OPSCompilerLib.jar" dist
@copy /Y "..\IDLParser\dist\IDLParser.jar" dist
@copy /Y "..\NBOPSIDLSupport\dist\IDLTemplates.jar" dist

@javac -cp "dist\ConfigurationLib.jar;dist\OPSJLib.jar;dist\IDLParser.jar;dist\OPSCompilerLib.jar;dist\IDLTemplates.jar" @"src/files.txt" -d "build/classes"

@jar cfme "dist/opsc.jar" "src/MANIFEST.MF"  "opsc/OpsCompiler" -C "build/classes/" .
@popd
