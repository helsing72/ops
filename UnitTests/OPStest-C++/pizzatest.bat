@pushd %~dp0

cd ..\..

build.debug\UnitTests\OPStest-C++\gtest\debug\gtest-main
build.debug\UnitTests\OPStest-C++\gtest\debug\gtest-mempool
build.debug\UnitTests\OPStest-C++\UnitTests\debug\test-serialize-and-deserialize

@rem run these in parallel
start /B build.debug\UnitTests\OPStest-C++\UnitTests\debug\test-subscribe
start /B build.debug\UnitTests\OPStest-C++\UnitTests\debug\test-subscribeTCP
start /B build.debug\UnitTests\OPStest-C++\UnitTests\debug\test-subscribeUDP

@timeout /T 2 /NOBREAK

build.debug\UnitTests\OPStest-C++\src\debug\publishBin

@ECHO OFF
rem wait for 'started' commands above to finish
:LOOP
tasklist | find /i "test-subscribeUDP" >nul 2>&1
IF ERRORLEVEL 1 (
  GOTO CONTINUE
) ELSE (
  timeout /T 2 /NOBREAK >nul
  GOTO LOOP
)

:CONTINUE

@popd
