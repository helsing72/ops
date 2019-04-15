@echo Building VerifySerDes ...
@javac -version
@IF NOT EXIST .obj (
	mkdir .obj
)
@IF NOT EXIST dist (
	mkdir dist
)
@copy /Y "..\..\..\Java\OPSJLib\dist\OPSJLib.jar" dist
@copy /Y "..\..\..\Libs\ConfigurationLib\dist\ConfigurationLib.jar" dist
@copy /Y "..\..\OPSIdls\TestAll\Generated\Java\TestAll.jar" dist

javac -cp "dist/OPSJLib.jar";"dist/ConfigurationLib.jar";"dist/TestAll.jar" @"src/files.txt" -d ".obj"

jar cfm "VerifySerDes.jar" "src/manifest_console_app.txt" -C ".obj" .
