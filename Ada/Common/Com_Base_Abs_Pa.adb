
with  Ada.Unchecked_Deallocation,
      Ada.Tags,
      Com_SyncPrimitives_Pa;

package body Com_Base_Abs_Pa is

-- ===========================================================================    
--          L o c a l   T y p e s   A n d   C o n s t a n t s
-- ===========================================================================    
  Count         : aliased Integer32 := 0;
  TraceRoutine  : TraceRoutine_At   := null;

-- ===========================================================================    
--          P u b l i c   R o u t i n e s
-- ===========================================================================    
  function NumActiveObjects return Integer32 is
  begin
    return Count;
  end;

  ---------------------------------------------------------------------------
  -- Get class name for object (May be overrided to shorten class name)
  ---------------------------------------------------------------------------
  function ClassName(Self : Com_Base_Abs_Class) return String is
  begin
    return Ada.Tags.External_Tag(Com_Base_Abs_Class'Class(Self)'Tag);
  end ClassName;

  --------------------------------------------------------------------------
  -- Unchecked deallocation for the Com_Base
  --------------------------------------------------------------------------
  procedure Dealloc is new Ada.Unchecked_Deallocation(  Com_Base_Abs_Class'Class,
                                                        Com_Base_Abs_Class_At);

  ----------------------------------------------------------------------
  -- Destructor ( There is no need to override this method anytime, use
  --              Finalize() instead to dealloc memory )
  ----------------------------------------------------------------------
  procedure Free(Self : access Com_Base_Abs_Class) is
    Dummy : Com_Base_Abs_Class_At;
    tmp : Integer32;
  begin
    Dummy := Com_Base_Abs_Class_At(Self);
    if Dummy /= null then
      tmp := Com_SyncPrimitives_Pa.InterlockedDecrement(Count'Access);
      if TraceRoutine /= null then
        TraceRoutine( Class         => OriginalClassName( Self.all ),
                      CreateStatus  => Dealloc,
                      TotalAllocObj => tmp);
      end if;
      Com_Base_Abs_Pa.Dealloc(Dummy);
    end if;
  end Free;
                      
  ----------------------------------------------------------------------
  -- Initialize object (Only used to trace allocation of object)
  ----------------------------------------------------------------------
  procedure Initialize(Self : in out Com_Base_Abs_Class) is
    tmp : Integer32;
  begin
    tmp := Com_SyncPrimitives_Pa.InterlockedIncrement(Count'Access);
    if TraceRoutine /= null then
      TraceRoutine( Class         => OriginalClassName( Self ),
                    CreateStatus  => Alloc,
                    TotalAllocObj => tmp);
    end if;
  end Initialize;

  ----------------------------------------------------------------------
  -- Install trace routine to catch allocation/deallocation
  -- of objects, when running.
  ----------------------------------------------------------------------
  procedure InstallTrace(Routine : TraceRoutine_At) is
  begin
    TraceRoutine := Routine;
  end InstallTrace;

  procedure UnInstallTrace(Routine : TraceRoutine_At) is
  begin
    TraceRoutine := null;
  end UnInstallTrace;

  ---------------------------------------------------------------------------
  -- Get original Class name (  Shall not be overrided to get the same
  --                            name when create/free object)
  ---------------------------------------------------------------------------
  function OriginalClassName(Self : Com_Base_Abs_Class) return String is
  begin
    return Ada.Tags.External_Tag(Com_Base_Abs_Class'Class(Self)'Tag);
  end OriginalClassName;
  
end Com_Base_Abs_Pa;

