mkdir C++

mkdir C++\include
mkdir C++\include\xml
mkdir C++\include\ops
mkdir C++\lib
mkdir C++\source
mkdir C++\source\xml


copy "..\C++\include\*.*" "C++\include"
copy "..\C++\include\xml\*.*" "C++\include\xml"
copy "..\C++\include\ops\*.*" "C++\include\ops"

copy "..\C++\source\*.*" "C++\source"
copy "..\C++\source\xml\*.*" "C++\source\xml"

copy "..\C++\lib\*.*" "C++\lib"
copy "C:\Program Files\boost\boost_1_38\lib\*.*" "C++\lib"


rem jar cvf OPS_Core_Cpp.jar C++
rem rename OPS_Core_Cpp.jar OPS_Core_Cpp.zip
