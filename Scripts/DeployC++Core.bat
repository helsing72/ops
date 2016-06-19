@rem find out where this script is
@set SCRIPT_PATH=%~dp0

@pushd %~dp0

mkdir Release
mkdir Release\C++

mkdir Release\C++\include
mkdir Release\C++\include\xml
mkdir Release\C++\source
mkdir Release\C++\source\xml
mkdir Release\C++\lib-vc90-x86
mkdir Release\C++\lib-vc90-x64
mkdir Release\C++\lib-vc100-x86
mkdir Release\C++\lib-vc100-x64

rem === COPY SOURCE ===
copy "..\Cpp\include\*.*" "Release\C++\include"
copy "..\Cpp\include\xml\*.*" "Release\C++\include\xml"

copy "..\Cpp\source\*.*" "Release\C++\source"
copy "..\Cpp\source\xml\*.*" "Release\C++\source\xml"


rem === COPY VS2008 LIBS===
copy "..\Cpp\lib-vc90-x86\*.*" "Release\C++\lib-vc90-x86"
copy "..\Cpp\lib-vc90-x64\*.*" "Release\C++\lib-vc90-x64"


rem === COPY BOOST LIBS VS2008 ===
copy "C:\Program\boost\boost_1_38\lib\*.*" "Release\C++\lib-vc90-x86"

copy "C:\Program\boost\boost_1_38\lib-x64\boost_date_time*.*" "Release\C++\lib-vc90-x64"
copy "C:\Program\boost\boost_1_38\lib-x64\boost_filesystem*.*" "Release\C++\lib-vc90-x64"
copy "C:\Program\boost\boost_1_38\lib-x64\boost_regex*.*" "Release\C++\lib-vc90-x64"
copy "C:\Program\boost\boost_1_38\lib-x64\boost_system*.*" "Release\C++\lib-vc90-x64"
copy "C:\Program\boost\boost_1_38\lib-x64\boost_thread*.*" "Release\C++\lib-vc90-x64"

copy "C:\Program\boost\boost_1_38\lib-x64\libboost_date_time*.*" "Release\C++\lib-vc90-x64"
copy "C:\Program\boost\boost_1_38\lib-x64\libboost_filesystem*.*" "Release\C++\lib-vc90-x64"
copy "C:\Program\boost\boost_1_38\lib-x64\libboost_regex*.*" "Release\C++\lib-vc90-x64"
copy "C:\Program\boost\boost_1_38\lib-x64\libboost_system*.*" "Release\C++\lib-vc90-x64"
copy "C:\Program\boost\boost_1_38\lib-x64\libboost_thread*.*" "Release\C++\lib-vc90-x64"


rem === COPY VS2010 LIBS===
copy "..\Cpp\lib-vc100-x86\*.*" "Release\C++\lib-vc100-x86"


rem === COPY BOOST LIBS VS2010 ===
copy "C:\Program\boost\boost_1_48\lib\boost_date_time*.*" "Release\C++\lib-vc100-x86"
copy "C:\Program\boost\boost_1_48\lib\boost_filesystem*.*" "Release\C++\lib-vc100-x86"
copy "C:\Program\boost\boost_1_48\lib\boost_regex*.*" "Release\C++\lib-vc100-x86"
copy "C:\Program\boost\boost_1_48\lib\boost_system*.*" "Release\C++\lib-vc100-x86"
copy "C:\Program\boost\boost_1_48\lib\boost_thread*.*" "Release\C++\lib-vc100-x86"

copy "C:\Program\boost\boost_1_48\lib\libboost_date_time*.*" "Release\C++\lib-vc100-x86"
copy "C:\Program\boost\boost_1_48\lib\libboost_filesystem*.*" "Release\C++\lib-vc100-x86"
copy "C:\Program\boost\boost_1_48\lib\libboost_regex*.*" "Release\C++\lib-vc100-x86"
copy "C:\Program\boost\boost_1_48\lib\libboost_system*.*" "Release\C++\lib-vc100-x86"
copy "C:\Program\boost\boost_1_48\lib\libboost_thread*.*" "Release\C++\lib-vc100-x86"

copy "C:\Program\boost\boost_1_48\lib-x64\boost_date_time*.*" "Release\C++\lib-vc100-x64"
copy "C:\Program\boost\boost_1_48\lib-x64\boost_filesystem*.*" "Release\C++\lib-vc100-x64"
copy "C:\Program\boost\boost_1_48\lib-x64\boost_regex*.*" "Release\C++\lib-vc100-x64"
copy "C:\Program\boost\boost_1_48\lib-x64\boost_system*.*" "Release\C++\lib-vc100-x64"
copy "C:\Program\boost\boost_1_48\lib-x64\boost_thread*.*" "Release\C++\lib-vc100-x64"

copy "C:\Program\boost\boost_1_48\lib-x64\libboost_date_time*.*" "Release\C++\lib-vc100-x64"
copy "C:\Program\boost\boost_1_48\lib-x64\libboost_filesystem*.*" "Release\C++\lib-vc100-x64"
copy "C:\Program\boost\boost_1_48\lib-x64\libboost_regex*.*" "Release\C++\lib-vc100-x64"
copy "C:\Program\boost\boost_1_48\lib-x64\libboost_system*.*" "Release\C++\lib-vc100-x64"
copy "C:\Program\boost\boost_1_48\lib-x64\libboost_thread*.*" "Release\C++\lib-vc100-x64"

pause

@popd
