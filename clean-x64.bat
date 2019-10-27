
@rem define build directories
@set BOOTSTRAP_DIR=%~dp0\build.bootstrap-x64
@set DBG_DIR=%~dp0\build.debug-x64
@set OPT_DIR=%~dp0\build.opt-x64
@set INSTALL_PREFIX=%~dp0\deploy-x64

rmdir /S /Q %BOOTSTRAP_DIR%
rmdir /S /Q %DBG_DIR%
rmdir /S /Q %OPT_DIR%
rmdir /S /Q %INSTALL_PREFIX%

rmdir /S /Q %~dp0\Common\idl\Generated
rmdir /S /Q %~dp0\Tools\OPSBridge\idl\Generated

del %~dp0\Cpp\include\OPSStringLengths.h
