@echo Building ConfigurationLib ...
@javac -version
@IF NOT EXIST "build/classes" (
	mkdir "build/classes"
)
@IF NOT EXIST dist (
	mkdir dist
)

@javac @"src/files.txt" -d "build/classes"

@jar cf "dist/ConfigurationLib.jar" -C "build/classes/" .
