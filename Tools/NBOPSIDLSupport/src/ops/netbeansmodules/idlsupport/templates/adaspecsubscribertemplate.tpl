-- Auto generated OPS-code. DO NOT MODIFY!
with Ops_Pa.OpsObject_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.Subscriber_Pa,
     __unitName,
     System;
use  Ops_Pa.OpsObject_Pa,
     Ops_Pa.OpsObject_Pa.Topic_Pa,
     Ops_Pa.Subscriber_Pa,
     __unitName,
     System;

package __subUnitName is

-- ==========================================================================
--      C l a s s    D e c l a r a t i o n.
-- ==========================================================================
  type __classNameSubscriber_Class is new Subscriber_Class with null record;
  type __classNameSubscriber_Class_At is access all __classNameSubscriber_Class'Class;

  function Create( t : Topic_Class_At ) return __classNameSubscriber_Class_At;

  -- Copies the latest received data into d
  -- Clears the "new data" flag (see newDataExist()).
  function getData( Self : in out __classNameSubscriber_Class; d : in out __className_Class_At) return Boolean;

  -- Returns a reference to the latest received data object.
  -- Clears the "new data" flag (see newDataExist()).
  -- NOTE: MessageLock should be hold while working with the data object, to
  -- prevent a new incoming message to delete the current one while in use.
  function getTypedDataReference( Self : in out __classNameSubscriber_Class ) return __className_Class_At;

private
  procedure InitInstance( Self : in out __classNameSubscriber_Class;
                          SelfAt : __classNameSubscriber_Class_At;
                          t : Topic_Class_At );

  --------------------------------------------------------------------------
  --  Finalize the object
  --  Will be called automatically when object is deleted.
  --------------------------------------------------------------------------
  procedure Finalize( Self : in out __classNameSubscriber_Class );

end __subUnitName;
