
with Ada.Finalization,
     Interfaces;
      
package Com_Base_Abs_Pa is

-- ===========================================================================    
--          C l a s s   D e c l a r a t i o n
-- ===========================================================================    
  type Com_Base_Abs_Class is abstract new 
          Ada.Finalization.Limited_Controlled with null record;
          
  type Com_Base_Abs_Class_At is access all Com_Base_Abs_Class'Class;

-- ===========================================================================    
--          P u b l i c   T y p e s   A n d   C o n s t a n t s
-- ===========================================================================    
  
-- ===========================================================================    
--          P u b l i c   R o u t i n e s
-- ===========================================================================    
  ---------------------------------------------------------------------------
  -- Get class name for object (May be overrided to shorten class name)
  ---------------------------------------------------------------------------
  function ClassName(Self : Com_Base_Abs_Class) return String;
  
  ----------------------------------------------------------------------
  -- Destructor ( There is no need to override this method anytime, use
  --              Finalize() instead to dealloc memory )
  ----------------------------------------------------------------------
  procedure Free(Self : access Com_Base_Abs_Class);

  ----------------------------------------------------------------------
  -- Destructor
  ----------------------------------------------------------------------
  overriding procedure Finalize(Self : in out Com_Base_Abs_Class) is abstract;
  
  ----------------------------------------------------------------------
  -- Initialize object (Only used to trace allocation of object)
  ----------------------------------------------------------------------
  overriding procedure Initialize(Self : in out Com_Base_Abs_Class);
  
  ----------------------------------------------------------------------
  -- Install/Uninstall trace routine to catch allocation/deallocation
  -- of objects, when running.
  ----------------------------------------------------------------------
  type CreateStatus_T is (Alloc, Dealloc);
  type TraceRoutine_At is access procedure( Class         : String;
                                            CreateStatus  : CreateStatus_T;
                                            TotalAllocObj : Interfaces.Integer_32);
  procedure InstallTrace(   Routine : TraceRoutine_At);
  procedure UnInstallTrace( Routine : TraceRoutine_At);
  
  -- Debug
  function NumActiveObjects return Interfaces.Integer_32;
  
private
  ---------------------------------------------------------------------------
  -- Get original Class name (  Shall not be overrided to get the same
  --                            name when create/free object)
  ---------------------------------------------------------------------------
  function OriginalClassName(Self : Com_Base_Abs_Class) return String;

end Com_Base_Abs_Pa;

