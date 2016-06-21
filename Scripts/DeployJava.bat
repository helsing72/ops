@rem find out where this script is
@set SCRIPT_PATH=%~dp0

@pushd %~dp0

mkdir Release
mkdir Release\Java

copy "..\Java\OPSJLib\dist\*.*" "Release\Java"
copy "..\Libs\ConfigurationLib\dist\*.*" "Release\Java"

pause

@popd
