@echo Building PizzaTest ...
@javac -version
@IF NOT EXIST .obj (
	mkdir .obj
)
@IF NOT EXIST dist (
	mkdir dist
)
@copy /Y "..\..\..\Java\OPSJLib\dist\OPSJLib.jar" dist
@copy /Y "..\..\..\Libs\ConfigurationLib\dist\ConfigurationLib.jar" dist
@copy /Y "..\..\OPSIdls\PizzaProject\Generated\Java\PizzaProject.jar" dist

javac -cp "dist/OPSJLib.jar";"dist/ConfigurationLib.jar";"dist/PizzaProject.jar" @"src/consoleapp/files.txt" -d ".obj"

jar cfm "PizzaTest.jar" "src/consoleapp/manifest_console_app.txt" -C ".obj" .
