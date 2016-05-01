@rem Generating IDL parser java source from IDL.jj using JavaCC
@set JAVACC=%~dp0\..\javacc-6.0\bin\javacc.bat
@pushd %~dp0\src\parsing\javaccparser
@call %JAVACC% IDL.jj
@popd
