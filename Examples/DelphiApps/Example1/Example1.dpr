program Example1;

{$APPTYPE CONSOLE}

{$R *.res}

//
// NOTE. This example requires that OPS has been built into a Delphi package
//

uses
  System.Win.ComObj,
  System.SysUtils,
  uExamplePublisher in 'uExamplePublisher.pas',
  TestAll.BaseData in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.BaseData.pas',
  TestAll.ChildData in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.ChildData.pas',
  TestAll.Fruit in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.Fruit.pas',
  TestAll.TestAllTypeFactory in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.TestAllTypeFactory.pas',
  TestAll.TestData in '..\..\OPSIdls\TestAll\Generated\Delphi\TestAll\TestAll.TestData.pas',
  uExampleSubscriber in 'uExampleSubscriber.pas';

procedure Usage;
begin
	writeln('');
  writeln('Usage: Example1 pub|sub_poll|sub_callback');
 	writeln('');
end;

var
  arg : string;
begin
{$IFDEF WIN32}
  ReportMemoryLeaksOnShutdown := True; //DebugHook <> 0;
{$ENDIF}
  CoInitializeEx(nil, 0);    // Needed for the TXMLDocument
  try
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
