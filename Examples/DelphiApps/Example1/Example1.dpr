program Example1;

{$APPTYPE CONSOLE}

{$R *.res}

//
// NOTE. This example requires that OPS has been built into a Delphi package
//

uses
  System.Win.ComObj,
  System.SysUtils,
  uOps.OPSConfigRepository,
  uExamplePublisher in 'uExamplePublisher.pas',
  TestAll.BaseData in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.BaseData.pas',
  TestAll.ChildData in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.ChildData.pas',
  TestAll.Fruit in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.Fruit.pas',
  TestAll.TestAllTypeFactory in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.TestAllTypeFactory.pas',
  TestAll.TestData in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.TestData.pas',
  uExampleSubscriber in 'uExampleSubscriber.pas',
  TestAll.NoData in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.NoData.pas',
  TestAll.Definitions in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.Definitions.pas';

procedure Usage;
begin
	writeln('');
  writeln('Usage: Example1 pub|sub_poll|sub_callback');
 	writeln('');
end;

var
  arg : string;
  cwd : string;
  idx : Integer;
begin
{$IFDEF WIN32}
  ReportMemoryLeaksOnShutdown := True; //DebugHook <> 0;
{$ENDIF}
  CoInitializeEx(nil, 0);    // Needed for the TXMLDocument
  try
    if FileExists('ops_config.xml') then begin
      Writeln('Using config file in CWD');
    end else begin
      cwd := GetCurrentDir;
      idx := Pos('Example', cwd);
      if Idx > 0 then begin
        cwd := Copy(cwd, 0, idx-1) + 'Examples/OPSIdls/TestAll/ops_config.xml';
        uOps.OPSConfigRepository.TOPSConfigRepository.Instance.Add(cwd);
        Writeln('Using config file: ' + cwd);
      end;
    end;

    if ParamCount >= 1 then begin
	  	arg := ParamStr(1);
  		if arg = 'pub' then begin
	  		PublisherExample();
		  end else if arg = 'sub_callback' then begin
  			CallbackSubscriberExample();
	  	end else if arg = 'sub_poll' then begin
		  	PollingSubscriberExample();
  		end else begin
	  		usage();
		  end;
  	end else begin
	  	Usage;
  	end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
