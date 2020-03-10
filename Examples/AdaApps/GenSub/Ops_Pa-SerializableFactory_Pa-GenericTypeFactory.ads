-- Auto generated OPS-code. DO NOT MODIFY!
with Ops_Pa.SerializableFactory_Pa;
use  Ops_Pa.SerializableFactory_Pa;

package Ops_Pa.SerializableFactory_Pa.GenericTypeFactory is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type GenericTypeFactory_Class is new SerializableFactory_Class with null record;
  type GenericTypeFactory_Class_At is access all GenericTypeFactory_Class'Class;

  -- Constructor
  function Create return GenericTypeFactory_Class_At;

  -- Create a serializable class instance from given type
  overriding function Make( Self : GenericTypeFactory_Class; types : string) return Serializable_Class_At;

--private

  procedure InitInstance( Self : in out GenericTypeFactory_Class );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out GenericTypeFactory_Class );

end;
