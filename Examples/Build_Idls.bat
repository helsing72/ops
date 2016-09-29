@pushd %~dp0
call ..\Tools\opsc\opsc.bat -P OPSIdls\ChatExample -g ALL
call ..\Tools\opsc\opsc.bat -P OPSIdls\HelloRequestReply -g ALL
call ..\Tools\opsc\opsc.bat -P OPSIdls\HelloWorld -g ALL
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\BaseIDLs -g ALL
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\DerivedIDLs -g ALL
call ..\Tools\opsc\opsc.bat -P OPSIdls\PizzaProject -g ALL
call ..\Tools\opsc\opsc.bat -P OPSIdls\TestAll -g ALL
call ..\Tools\opsc\opsc.bat -P OPSIdls\WeatherStationExample -g ALL
@popd
