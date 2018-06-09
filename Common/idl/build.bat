@pushd %~dp0
@call ..\..\Tools\opsc\opsc.bat -g ALL -B ALL -o Generated *.idl -p opsidls
@popd
