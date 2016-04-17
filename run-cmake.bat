@rem NOTE requires a MSBUILD command shell to work

@rem define build directories
@set DBG_DIR=%~dp0\build.debug
@set OPT_DIR=%~dp0\build.opt

@rem build and install Debug
@pushd %~dp0
@IF NOT EXIST %DBG_DIR% mkdir %DBG_DIR%
@cd %DBG_DIR%
cmake -DCMAKE_BUILD_TYPE=Debug ..
cmake --build . --target ALL_BUILD --config Debug
cmake --build . --target INSTALL --config Debug
@popd

@rem build and install Release
@pushd %~dp0
@IF NOT EXIST %OPT_DIR% mkdir %OPT_DIR%
@cd %OPT_DIR%
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build . --target ALL_BUILD --config Release
cmake --build . --target INSTALL --config Release
@popd
