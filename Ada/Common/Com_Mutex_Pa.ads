
with  Ada.Finalization;

package Com_Mutex_Pa is

  protected type Mutex is

    entry Acquire;
    procedure Release;

  private
    Owned : Boolean := False;
  end Mutex;

  type Scope_Lock(Lock : access Mutex) is tagged limited private;

  -- Example usage:
  --   Mtx : aliased Mutex;
  --   ...
  --   procedure Op is
  --     S : Scope_Lock(Mtx'Access);   <-- will acquire Mtx at entry and release at exit from scope
  --   begin
  --     do operations;
  --   end;

private
  type Scope_Lock(Lock : access Mutex) is
    new Ada.Finalization.Limited_Controlled with null record;

  overriding procedure Initialize(Self : in out Scope_Lock);
  overriding procedure Finalize(Self : in out Scope_Lock);

end Com_Mutex_Pa;

