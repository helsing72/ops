program Example1;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  uLogger in '..\..\..\Delphi\Common\uLogger.pas',
  uNotifier in '..\..\..\Delphi\Common\uNotifier.pas',
  uRunner in '..\..\..\Delphi\Common\uRunner.pas',
  uSockets in '..\..\..\Delphi\Common\uSockets.pas',
  uOps.ArchiverInOut in '..\..\..\Delphi\Source\uOps.ArchiverInOut.pas',
  uOps.ByteBuffer in '..\..\..\Delphi\Source\uOps.ByteBuffer.pas',
  uOps.DeadlineTimer in '..\..\..\Delphi\Source\uOps.DeadlineTimer.pas',
  uOps.Domain in '..\..\..\Delphi\Source\uOps.Domain.pas',
  uOps.Error in '..\..\..\Delphi\Source\uOps.Error.pas',
  uOps.Exceptions in '..\..\..\Delphi\Source\uOps.Exceptions.pas',
  uOps.FilterQoSPolicy in '..\..\..\Delphi\Source\uOps.FilterQoSPolicy.pas',
  uOps.KeyFilterQoSPolicy in '..\..\..\Delphi\Source\uOps.KeyFilterQoSPolicy.pas',
  uOps.MemoryMap in '..\..\..\Delphi\Source\uOps.MemoryMap.pas',
  uOps.OpsArchiverIn in '..\..\..\Delphi\Source\uOps.OpsArchiverIn.pas',
  uOps.OpsArchiverOut in '..\..\..\Delphi\Source\uOps.OpsArchiverOut.pas',
  uOps.OPSConfig in '..\..\..\Delphi\Source\uOps.OPSConfig.pas',
  uOps.OPSConfigRepository in '..\..\..\Delphi\Source\uOps.OPSConfigRepository.pas',
  uOps.OpsMessage in '..\..\..\Delphi\Source\uOps.OpsMessage.pas',
  uOps.OpsObject in '..\..\..\Delphi\Source\uOps.OpsObject.pas',
  uOps.OPSObjectFactory in '..\..\..\Delphi\Source\uOps.OPSObjectFactory.pas',
  uOps.Participant in '..\..\..\Delphi\Source\uOps.Participant.pas',
  uOps.ParticipantInfoData in '..\..\..\Delphi\Source\uOps.ParticipantInfoData.pas',
  uOps.ParticipantInfoDataListener in '..\..\..\Delphi\Source\uOps.ParticipantInfoDataListener.pas',
  uOps.PrintArchiverOut in '..\..\..\Delphi\Source\uOps.PrintArchiverOut.pas',
  uOps.Publisher in '..\..\..\Delphi\Source\uOps.Publisher.pas',
  uOps.PublisherAbs in '..\..\..\Delphi\Source\uOps.PublisherAbs.pas',
  uOps.SerializableCompositeFactory in '..\..\..\Delphi\Source\uOps.SerializableCompositeFactory.pas',
  uOps.SerializableFactory in '..\..\..\Delphi\Source\uOps.SerializableFactory.pas',
  uOps.SerializableInheritingTypeFactory in '..\..\..\Delphi\Source\uOps.SerializableInheritingTypeFactory.pas',
  uOps.Subscriber in '..\..\..\Delphi\Source\uOps.Subscriber.pas',
  uOps.TimeHelper in '..\..\..\Delphi\Source\uOps.TimeHelper.pas',
  uOps.Topic in '..\..\..\Delphi\Source\uOps.Topic.pas',
  uOps.TopicInfoData in '..\..\..\Delphi\Source\uOps.TopicInfoData.pas',
  uOps.Types in '..\..\..\Delphi\Source\uOps.Types.pas',
  uOps.Utilities in '..\..\..\Delphi\Source\uOps.Utilities.pas',
  uOps.XMLArchiverIn in '..\..\..\Delphi\Source\uOps.XMLArchiverIn.pas',
  uOps.XMLArchiverOut in '..\..\..\Delphi\Source\uOps.XMLArchiverOut.pas',
  uOps.Transport.McSendDataHandler in '..\..\..\Delphi\Source\Transport\uOps.Transport.McSendDataHandler.pas',
  uOps.Transport.McUdpSendDataHandler in '..\..\..\Delphi\Source\Transport\uOps.Transport.McUdpSendDataHandler.pas',
  uOps.Transport.MulticastReceiver in '..\..\..\Delphi\Source\Transport\uOps.Transport.MulticastReceiver.pas',
  uOps.Transport.ReceiveDataHandler in '..\..\..\Delphi\Source\Transport\uOps.Transport.ReceiveDataHandler.pas',
  uOps.Transport.ReceiveDataHandlerFactory in '..\..\..\Delphi\Source\Transport\uOps.Transport.ReceiveDataHandlerFactory.pas',
  uOps.Transport.Receiver in '..\..\..\Delphi\Source\Transport\uOps.Transport.Receiver.pas',
  uOps.Transport.SendDataHandler in '..\..\..\Delphi\Source\Transport\uOps.Transport.SendDataHandler.pas',
  uOps.Transport.SendDataHandlerFactory in '..\..\..\Delphi\Source\Transport\uOps.Transport.SendDataHandlerFactory.pas',
  uOps.Transport.Sender in '..\..\..\Delphi\Source\Transport\uOps.Transport.Sender.pas',
  uOps.Transport.TCPClient in '..\..\..\Delphi\Source\Transport\uOps.Transport.TCPClient.pas',
  uOPs.Transport.TCPSendDataHandler in '..\..\..\Delphi\Source\Transport\uOPs.Transport.TCPSendDataHandler.pas',
  uOps.Transport.TCPServer in '..\..\..\Delphi\Source\Transport\uOps.Transport.TCPServer.pas',
  uOps.Transport.UDPReceiver in '..\..\..\Delphi\Source\Transport\uOps.Transport.UDPReceiver.pas',
  uOps.Transport.UDPSender in '..\..\..\Delphi\Source\Transport\uOps.Transport.UDPSender.pas',
  uOps.RequestReply in '..\..\..\Delphi\Source\RequestReply\uOps.RequestReply.pas',
  uOps.RequestReply.Reply in '..\..\..\Delphi\Source\RequestReply\uOps.RequestReply.Reply.pas',
  uOps.RequestReply.Request in '..\..\..\Delphi\Source\RequestReply\uOps.RequestReply.Request.pas',
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
