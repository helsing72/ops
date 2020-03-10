with
  Ops_Pa.OpsObject_Pa,
  System;
use
  System;

package body Ops_Pa.SerializableFactory_Pa.GenericTypeFactory is

  -- Constructors
  function Create return GenericTypeFactory_Class_At is
    Self : GenericTypeFactory_Class_At := null;
  begin
    Self := new GenericTypeFactory_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  -- Create a serializable class instance from given type
  overriding function Make( Self : GenericTypeFactory_Class; types : string) return Serializable_Class_At is
  begin
    if types /= "" then
      return Serializable_Class_At(Ops_Pa.OpsObject_Pa.Create);
    end if;

    return null;
  end;

  procedure InitInstance( Self : in out GenericTypeFactory_Class ) is
  begin
    null;
  end;

  overriding procedure Finalize( Self : in out GenericTypeFactory_Class ) is
  begin
    null;
  end;

end;
