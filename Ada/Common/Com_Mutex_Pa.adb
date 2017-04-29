
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

  procedure Initialize(Self : in out Scope_Lock) is
  begin
    Self.Lock.Acquire;
  end;

  procedure Finalize(Self : in out Scope_Lock) is
  begin
    Self.Lock.Release;
  end;

end Com_Mutex_Pa;
