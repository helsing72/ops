@pushd %~dp0
call ..\Tools\opsc\opsc.bat -P OPSIdls\ChatExample
call ..\Tools\opsc\opsc.bat -P OPSIdls\HelloRequestReply
call ..\Tools\opsc\opsc.bat -P OPSIdls\HelloWorld
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\BaseIDLs
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\DerivedIDLs
call ..\Tools\opsc\opsc.bat -P OPSIdls\PizzaProject
call ..\Tools\opsc\opsc.bat -P OPSIdls\TestAll
call ..\Tools\opsc\opsc.bat -P OPSIdls\WeatherStationExample
@popd
