@echo Building OPSJLib ...
@javac -version
@IF NOT EXIST "build/classes" (
	mkdir "build/classes"
)
@IF NOT EXIST dist (
	mkdir dist
)

@javac -cp "../../Libs/ConfigurationLib/dist/ConfigurationLib.jar" @"src/files.txt" -d "build/classes"

@jar cf "dist/OPSJLib.jar" -C "build/classes/" .
