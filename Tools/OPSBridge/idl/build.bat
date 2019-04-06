@pushd %~dp0
@call ..\..\opsc\opsc.bat -g ALL -B ALL -o Generated *.idl -p opsbridge
@popd
