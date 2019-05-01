@pushd %~dp0
call ..\Tools\opsc\opsc.bat -P OPSIdls\ChatExample -g ALL -gpr ../../../../../Ada/
call ..\Tools\opsc\opsc.bat -P OPSIdls\HelloRequestReply -g ALL -gpr ../../../../../Ada/
call ..\Tools\opsc\opsc.bat -P OPSIdls\HelloWorld -g ALL -gpr ../../../../../Ada/
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\BaseIDLs -g ALL -gpr ../../../../../../Ada/
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\DerivedIDLs -g ALL -gpr ../../../../../../Ada/
call ..\Tools\opsc\opsc.bat -P OPSIdls\PizzaProject -g ALL -gpr ../../../../../Ada/
call ..\Tools\opsc\opsc.bat -P OPSIdls\TestAll -g ALL -gpr ../../../../../Ada/
call ..\Tools\opsc\opsc.bat -P OPSIdls\WeatherStationExample -g ALL -gpr ../../../../../Ada/
@popd
