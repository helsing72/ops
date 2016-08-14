//Auto generated OPS-code. DO NOT MODIFY!
unit __packageName.__className;

interface

uses
  uOps.Types,
  uOps.OpsObject,
  uOps.ArchiverInOut;

type
  __className = class(TOPSObject)
  public
    ///-----------------------------------------------------------------------
    type
      enum = (__declarations);

  public
    Value : enum;

    ///-----------------------------------------------------------------------
    constructor Create; overload;
    destructor Destroy; override;

    class function getTypeName : string;

    ///This method acceptes an ops::ArchiverInOut visitor which will serialize or deserialize an
    ///instance of this class to a format dictated by the implementation of the ArchiverInout.
    procedure Serialize(archiver : TArchiverInOut); override;

		//Returns a newely allocated deep copy/clone of this object.
		function Clone : TOPSObject; override;

		//Fills the parameter obj with all values from this object.
		procedure FillClone(var obj : TOPSObject); override;
  end;

implementation

uses SysUtils;

{ __className }

constructor __className.Create;
begin
  inherited Create;
  AppendType('__packageName.__className');
end;

destructor __className.Destroy;
begin
  inherited;
end;

class function __className.getTypeName : string;
begin
  Result := '__packageName.__className';
end;

procedure __className.Serialize(archiver : TArchiverInOut);
var
  val : Integer;
begin
	inherited Serialize(archiver);
  val := Integer(Value);
  archiver.inout('Value', val);
  Value := enum(val);
end;

///Returns a newely allocated deep copy/clone of this object.
function __className.Clone : TOPSObject;
begin
	Result := __className.Create;
  Self.FillClone(Result);
end;

///Fills the parameter obj with all values from this object.
procedure __className.FillClone(var obj : TOPSObject);
begin
	inherited fillClone(obj);
  with obj as __className do begin
    Value := Self.Value;
	end;
end;

end.
