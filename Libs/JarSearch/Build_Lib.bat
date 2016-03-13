@echo Building JarSearch ...
@javac -version
@IF NOT EXIST "build/classes" (
	mkdir "build/classes"
)
@IF NOT EXIST dist (
	mkdir dist
)

@javac @"src/files.txt" -d "build/classes"

@jar cf "dist/JarSearch.jar" -C "build/classes/" .
