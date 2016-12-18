
with Ctv; use Ctv;

package Com_SyncPrimitives_Pa is
  
  --------------------------------------------------------------------------
  -- Increments the variable in a task safe manner.
  -- Returns:
  --     > 0  Value after increment is > 0
  --     = 0  Value after increment is = 0
  --     < 0  Value after increment is < 0
  --------------------------------------------------------------------------
  function InterlockedIncrement (Target : access Integer32) return Integer32;
  pragma Import(Stdcall, InterlockedIncrement, "InterlockedIncrement");
  
  --------------------------------------------------------------------------
  -- Decrements the variable in a task safe manner.
  -- Returns:
  --     > 0  Value after decrement is > 0
  --     = 0  Value after decrement is = 0
  --     < 0  Value after decrement is < 0
  --------------------------------------------------------------------------
  function InterlockedDecrement (Target : access Integer32) return Integer32;
  pragma Import(Stdcall, InterlockedDecrement, "InterlockedDecrement");
  
  --------------------------------------------------------------------------
  -- Exchanges a pair of 32 bit values in a task safe manner.
  -- Returns the old value
  --------------------------------------------------------------------------
  function InterlockedExchange (Target   : access Unsigned_Int32;
                                NewValue : Unsigned_Int32) return Unsigned_Int32;
  pragma Import(Stdcall, InterlockedExchange, "InterlockedExchange");
  
end Com_SyncPrimitives_Pa;


