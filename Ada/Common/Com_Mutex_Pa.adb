
package body Com_Mutex_Pa is

  protected body Mutex is

    entry Acquire when not Owned is
    begin
      Owned := True;
    end Acquire;

    procedure Release is
    begin
      Owned := False;
    end Release;

  end Mutex;

end Com_Mutex_Pa;
