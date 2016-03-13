@echo Building OPSCompilerLib ...
@javac -version
@IF NOT EXIST "build/classes" (
	mkdir "build/classes"
)
@IF NOT EXIST dist (
	mkdir dist
)

@javac @"src/files.txt" -d "build/classes"

@jar cf "dist/OPSCompilerLib.jar" -C "build/classes/" .
