@pushd %~dp0
@call :STARTTEST CSharpApps\TestAll\ bin\debug\testall.exe "start"
@call :STARTTEST AdaApps\TestAll\ verifyserdes_main.exe "start"
@call :STARTTEST CppApps\TestAllC++Test\TestAllC++Test\ debug\verifyserdes.exe "start"
@call :STARTTEST DelphiApps\TestAll\ win32\debug\verifyserdesdelphi.exe "start"
@call :STARTTEST JavaApps\TestAll\ VerifySerDes.jar "start java -jar"
@call :STARTTEST PythonApps\TestAll\ run.bat "call"
@popd
@GOTO :EOF

@rem ------- Args: relpath exepath cmd
:STARTTEST
@pushd .
@cd %1
if exist %2 ( %~3 %2 )
@popd
