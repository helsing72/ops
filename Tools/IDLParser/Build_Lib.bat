@pushd %~dp0
@echo Building IDLParser ...
@javac -version
@IF NOT EXIST "build/classes" (
	mkdir "build/classes"
)
@IF NOT EXIST dist (
	mkdir dist
)

@javac -cp "../OPSCompilerLib/dist/OPSCompilerLib.jar" @"src/files.txt" -d "build/classes"

@jar cf "dist/IDLParser.jar" -C "build/classes/" .
@popd
