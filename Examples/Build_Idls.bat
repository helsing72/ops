@pushd %~dp0
@set OPS4_GPR_REL_PATH=../../../../../Ada/
call ..\Tools\opsc\opsc.bat -P OPSIdls\ChatExample -g ALL -gpr %OPS4_GPR_REL_PATH%
call ..\Tools\opsc\opsc.bat -P OPSIdls\HelloRequestReply -g ALL -gpr %OPS4_GPR_REL_PATH%
call ..\Tools\opsc\opsc.bat -P OPSIdls\HelloWorld -g ALL -gpr %OPS4_GPR_REL_PATH%
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\BaseIDLs -g ALL -gpr %OPS4_GPR_REL_PATH%
call ..\Tools\opsc\opsc.bat -P OPSIdls\MultiLibs\DerivedIDLs -g ALL -gpr %OPS4_GPR_REL_PATH%
call ..\Tools\opsc\opsc.bat -P OPSIdls\PizzaProject -g ALL -gpr %OPS4_GPR_REL_PATH%
call ..\Tools\opsc\opsc.bat -P OPSIdls\TestAll -g ALL -gpr %OPS4_GPR_REL_PATH%
call ..\Tools\opsc\opsc.bat -P OPSIdls\WeatherStationExample -g ALL -gpr %OPS4_GPR_REL_PATH%
@popd
