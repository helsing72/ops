
mkdir ..\Release
mkdir ..\Release\C#
mkdir ..\Release\C#\Debug
mkdir ..\Release\C#\Release
mkdir ..\Release\C#\Source
mkdir ..\Release\C#\Source\Archiver
mkdir ..\Release\C#\Source\Utilities

Copy "..\CSharp\Ops\*.cs" "..\Release\C#\Source"
Copy "..\CSharp\Ops\Archiver\*.cs" "..\Release\C#\Source\Archiver"
Copy "..\CSharp\Ops\Utilities\*.cs" "..\Release\C#\Source\Utilities"

Copy "..\CSharp\Projects\OpsLibrary\OpsLibrary\bin\debug\OpsLibrary.dll" "..\Release\C#\Debug"
Copy "..\CSharp\Projects\OpsLibrary\OpsLibrary\bin\debug\OpsLibrary.dll" "..\Release\C#\Release"

pause
