
with Text_IO; use Text_IO;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Directories; use Ada.Directories;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;

with Ops_Pa.OpsObject_Pa.OPSConfig_Pa; use Ops_Pa.OpsObject_Pa.OPSConfig_Pa;

with ExamplePublisher_Pa; use ExamplePublisher_Pa;
with ExampleSubscriber_Pa; use ExampleSubscriber_Pa;

procedure Example1 is

  procedure Usage is
  begin
    Put_Line("");
    Put_Line("Usage: Example1 pub|sub_poll|sub_callback");
    Put_Line("");
  end;

  procedure setup_alt_config(cfg_rel_ops4 : String) is
  begin
    -- Check if we have an ops_config.xml in CWD
    if Exists("ops_config.xml") then
      Put_Line("Using config file in CWD");

    else
      declare
        cwd : String := Current_Directory;
        pos : Natural := Index(cwd, "Examples", 1);
        dummy : Boolean;
      begin
        if pos > 1 then
          dummy := RepositoryInstance.add(Head(cwd, pos - 1) & cfg_rel_ops4);
          Put_Line("Using config file: " & Head(cwd, pos - 1) & cfg_rel_ops4);
        end if;
      end;
    end if;
  end;

begin
  begin
    setup_alt_config("Examples/OPSIdls/TestAll/ops_config.xml");
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

