// Auto generated OPS-code. DO NOT MODIFY!
unit __packageName.__className;

interface

uses
  uOps.Types,
  uOps.OpsObject,
  uOps.ArchiverInOut,
__importUnits
  uOps.SerializableFactory;

type
  __className = class(TSerializableFactory)
  public
    // Named 'Make' instead of 'Create' which in Delphi normaly is used for constructors
    function Make(types : string) : TSerializable; override;
  end;

implementation

function __className.Make(types : string) : TSerializable;
begin
  Result := nil;
__createMakeBody
end;

end.
