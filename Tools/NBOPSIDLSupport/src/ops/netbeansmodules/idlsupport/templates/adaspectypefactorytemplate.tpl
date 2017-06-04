-- Auto generated OPS-code. DO NOT MODIFY!
with Ops_Pa.SerializableFactory_Pa;
use  Ops_Pa.SerializableFactory_Pa;

package __unitName is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type __className_Class is new SerializableFactory_Class with null record;
  type __className_Class_At is access all __className_Class'Class;

  -- Constructor
  function Create return __className_Class_At;

  -- Create a serializable class instance from given type
  overriding function Make( Self : __className_Class; types : string) return Serializable_Class_At;

--private

  procedure InitInstance( Self : in out __className_Class );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out __className_Class );

end;
