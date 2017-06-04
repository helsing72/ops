-- Auto generated OPS-code. DO NOT MODIFY!

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

  overriding procedure Serialize( Self : in out __className_Class; archiver : ArchiverInOut_Class_At) is
    val : Int32;
  begin
    Serialize( OpsObject_Class(Self), archiver );
    val := enum'Pos(Self.Value);
    archiver.inout("Value", val);
    Self.Value := enum'Val(val);
  exception
    when Constraint_Error => null;    -- TODO
  end;

  -- Returns a newely allocated deep copy/clone of Self.
  overriding function Clone( Self : __className_Class ) return OpsObject_Class_At is
    Result : __className_Class_At := null;
  begin
    Result := Create;
    Self.FillClone( OpsObject_Class_At(Result) );
    return OpsObject_Class_At(Result);
  end Clone;

  -- Fills the parameter obj with all values from Self.
  overriding procedure FillClone( Self : __className_Class; obj : OpsObject_Class_At ) is
  begin
    FillClone( OpsObject_Class(Self), obj );
    if obj.all in __className_Class'Class then
      __className_Class(obj.all).Value := Self.Value;
    end if;
  end;

  procedure InitInstance( Self : in out __className_Class ) is
  begin
    InitInstance( OpsObject_Class(Self) );
    AppendType( OpsObject_Class(Self), TypeName_C );
  end;

  overriding procedure Finalize( Self : in out __className_Class ) is
  begin
    Finalize( OpsObject_Class(Self) );
  end;

  procedure Clear( Arr : in out __className_Class_At_Arr) is
  begin
    for i in Arr'Range loop
      if Arr(i) /= null then
        Free(Arr(i));
      end if;
    end loop;
  end;

end __unitName;
