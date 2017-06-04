-- Auto generated OPS-code. DO NOT MODIFY!
with Ops_Pa.OpsObject_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.PublisherAbs_Pa.Publisher_Pa,
     __unitName,
     System;
use  Ops_Pa.OpsObject_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.PublisherAbs_Pa.Publisher_Pa,
     __unitName,
     System;

package __pubUnitName is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type __classNamePublisher_Class is new Publisher_Class with null record;
  type __classNamePublisher_Class_At is access all __classNamePublisher_Class'Class;

  function Create( t : Topic_Class_At ) return __classNamePublisher_Class_At;

  procedure write( Self : in out __classNamePublisher_Class; data : __className_Class_At );

private
  procedure InitInstance( Self : in out __classNamePublisher_Class;
                          SelfAt : __classNamePublisher_Class_At;
                          t : Topic_Class_At );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  overriding procedure Finalize( Self : in out __classNamePublisher_Class );

end __pubUnitName;
