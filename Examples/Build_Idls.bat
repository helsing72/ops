@pushd %~dp0
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\BaseIDLs
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\DerivedIDLs
call ..\Tools\opsc\opsc.bat -P OPSIdls\PizzaProject
call ..\Tools\opsc\opsc.bat -P OPSIdls\TestAll
@popd
