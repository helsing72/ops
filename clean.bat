
@rem define build directories
@set BOOTSTRAP_DIR=%~dp0\build.bootstrap
@set DBG_DIR=%~dp0\build.debug
@set OPT_DIR=%~dp0\build.opt
@set INSTALL_PREFIX=%~dp0\deploy

rmdir /S /Q %BOOTSTRAP_DIR%
rmdir /S /Q %DBG_DIR%
rmdir /S /Q %OPT_DIR%
rmdir /S /Q %INSTALL_PREFIX%

rmdir /S /Q %~dp0\Common\idl\Generated
