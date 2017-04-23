
with Text_IO; use Text_IO;
with Ada.Command_Line; use Ada.Command_Line;

with ExamplePublisher_Pa; use ExamplePublisher_Pa;
with ExampleSubscriber_Pa; use ExampleSubscriber_Pa;

procedure Example1 is

  procedure Usage is
  begin
    Put_Line("");
    Put_Line("Usage: Example1 pub|sub_poll|sub_callback");
    Put_Line("");
  end;

begin
  begin
    if Argument_Count >= 1 then
      declare
        arg : String := Argument(1);
      begin
        if arg = "pub" then
          PublisherExample;
        elsif arg = "sub_callback" then
          CallbackSubscriberExample;
        elsif arg = "sub_poll" then
          PollingSubscriberExample;
        else
          usage;
        end if;
      end;
    else
      Usage;
    end if;
  exception
    when others =>
      Put_Line("Exception");
  end;
end;

