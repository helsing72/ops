@rem find out where this is script is so we can invoke jar in ../jars
@set SCRIPT_PATH=%~dp0

@echo Invoking script at: %SCRIPT_PATH%
@echo Current working directory: %CD%

@java -jar %SCRIPT_PATH%/dist/opsc.jar %*
