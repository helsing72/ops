
#define AppName "OPS IDL Builder"
; #define AppVersion GetFileVersion(AddBackslash(SourcePath) + "..\app.exe")
#define AppVersion "2016.06.18"

[Setup]
AppName={#AppName}
AppVerName={#AppName} version {#AppVersion}
;AppCopyright=Copyright � 2008-2016 .
DefaultDirName={pf}\OPS\{#AppName}
DefaultGroupName={#AppName}
; UninstallDisplayIcon={app}\app.exe
OutputDir=Release\Tools
OutputBaseFilename=OPS_IDL_Builder_{#AppVersion}_Setup
ShowLanguageDialog=no
PrivilegesRequired=none
VersionInfoVersion={#AppVersion}

[Files]
Source: Tools\OPS IDL Builder\*; DestDir: {app}; Flags: ignoreversion recursesubdirs

[Registry]

[Tasks]

[Icons]
Name: {group}\OPS IDL Builder; Filename: {app}\bin\ops_idl_builder_nb.exe

[Run]

[UninstallRun]

[_ISTool]
Use7zip=false

[Languages]
Name: English; MessagesFile: compiler:Default.isl
