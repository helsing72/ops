-- Auto generated OPS-code. DO NOT MODIFY!

package body __pubUnitName is

  function Create( t : Topic_Class_At ) return __classNamePublisher_Class_At is
    Self : __classNamePublisher_Class_At := null;
  begin
    Self := new __classNamePublisher_Class;
    InitInstance( Self.all, Self, t );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  procedure write( Self : in out __classNamePublisher_Class; data : __className_Class_At ) is
  begin
    WriteOPSObject( Publisher_Class(Self), OpsObject_Class_At(data) );
  end;

  procedure InitInstance( Self : in out __classNamePublisher_Class;
                          SelfAt : __classNamePublisher_Class_At;
                          t : Topic_Class_At ) is
  begin
    InitInstance( Publisher_Class(Self), Publisher_Class_At(SelfAt), t );
  end;

  overriding procedure Finalize( Self : in out __classNamePublisher_Class ) is
  begin
    Finalize( Publisher_Class(Self) );
  end;

end __pubUnitName;
