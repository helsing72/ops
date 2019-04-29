@pushd %~dp0
@call ..\..\Tools\opsc\opsc.bat -g ALL -B ALL -gpr ../../../../Ada/ -o Generated *.idl -p opsidls
@popd
