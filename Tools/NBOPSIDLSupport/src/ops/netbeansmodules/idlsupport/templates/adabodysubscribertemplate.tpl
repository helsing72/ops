-- Auto generated OPS-code. DO NOT MODIFY!

package body __subUnitName is

  function Create( t : Topic_Class_At ) return __classNameSubscriber_Class_At is
    Self : __classNameSubscriber_Class_At := null;
  begin
    Self := new __classNameSubscriber_Class;
    InitInstance( Self.all, Self, t );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  -- Copies the latest received data into d
  -- Clears the "new data" flag (see newDataExist()).
  function getData( Self : in out __classNameSubscriber_Class; d : in out __className_Class_At) return Boolean is
    Result : Boolean := False;
  begin
    if Self.Data /= null then
      declare
        -- Hold MessageLock during cloning of object
        S : Scope_MessageLock( Self.SelfAt );
      begin
        getData( Subscriber_Class(Self) ).FillClone( OPSObject_Class_At( d ));
      end;
      Result := True;
    end if;
    return Result;
  end;

  -- Returns a reference to the latest received data object.
  -- Clears the "new data" flag (see newDataExist()).
  -- NOTE: MessageLock should be held while working with the data object, to
  -- prevent a new incoming message to delete the current one while in use.
  function getTypedDataReference( Self : in out __classNameSubscriber_Class ) return __className_Class_At is
  begin
    return __className_Class_At( Self.getDataReference );
  end;

  procedure InitInstance( Self : in out __classNameSubscriber_Class;
                          SelfAt : __classNameSubscriber_Class_At;
                          t : Topic_Class_At ) is
  begin
    InitInstance( Subscriber_Class(Self), Subscriber_Class_At(SelfAt), t );
  end;

  procedure Finalize( Self : in out __classNameSubscriber_Class ) is
  begin
    Finalize( Subscriber_Class(Self) );
  end;

end __subUnitName;
