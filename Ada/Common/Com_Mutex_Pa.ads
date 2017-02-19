
package Com_Mutex_Pa is

  protected type Mutex is

    entry Acquire;
    procedure Release;

  private
    Owned : Boolean := False;
  end Mutex;

end Com_Mutex_Pa;

