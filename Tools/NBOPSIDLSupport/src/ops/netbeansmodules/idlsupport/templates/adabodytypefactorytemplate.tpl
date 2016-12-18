-- Auto generated OPS-code. DO NOT MODIFY!
with
__importUnits
  System;
use
__importUnits
  System;

package body __unitName is

  -- Constructors
  function Create return __className_Class_At is
    Self : __className_Class_At := null;
  begin
    Self := new __className_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  -- Create a serializable class instance from given type
  function Make( Self : __className_Class; types : string) return Serializable_Class_At is
  begin
__createMakeBody
    return null;
  end;

  procedure InitInstance( Self : in out __className_Class ) is
  begin
    null;
  end;

  procedure Finalize( Self : in out __className_Class ) is
  begin
    null;
  end;

end;
