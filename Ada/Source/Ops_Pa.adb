--
-- Copyright (C) 2016-2018 Lennart Andersson.
--  
-- This file is part of OPS (Open Publish Subscribe).
--  
-- OPS (Open Publish Subscribe) is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Lesser General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--  
-- OPS (Open Publish Subscribe) is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Lesser General Public License for more details.
--  
-- You should have received a copy of the GNU Lesser General Public License
-- along with OPS (Open Publish Subscribe).  If not, see <http://www.gnu.org/licenses/>.

with Ada.Text_IO,
     Ada.Real_Time,
     Ada.Exceptions,
     Ada.Unchecked_Conversion,
     Ada.Unchecked_Deallocation,
     Ada.Tags;

package body Ops_Pa is

  StartTime     : Float64;
  Count         : aliased System.Atomic_Counters.Atomic_Unsigned := 0;
  TraceRoutine  : TraceRoutine_At   := null;
  TraceProc     : TraceProc_At      := null;
  
  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function NumActiveObjects return System.Atomic_Counters.Atomic_Unsigned is
  begin
    return Count;
  end;

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

  procedure InstallTrace( Proc : TraceProc_At ) is
  begin
    TraceProc := Proc;
  end;
  
  ---------------------------------------------------------------------------
  -- Get original Class name (  Shall not be overridden to get the same
  --                            name when create/free object)
  ---------------------------------------------------------------------------
  function OriginalClassName(Self : Ops_Class) return String is
  begin
    return Ada.Tags.External_Tag(Ops_Class'Class(Self)'Tag);
  end OriginalClassName;
  
  --------------------------------------------------------------------------
  -- Get class name for object (May be overridden to shorten class name)
  --------------------------------------------------------------------------
  function ClassName(Self : Ops_Class) return String is
  begin
    return Ada.Tags.External_Tag(Ops_Class'Class(Self)'Tag);
  end ClassName;

  --------------------------------------------------------------------------
  -- Unchecked deallocation for the Com_Base
  --------------------------------------------------------------------------
  procedure Dealloc is new Ada.Unchecked_Deallocation( Ops_Class'Class, Ops_Class_At);

  ----------------------------------------------------------------------
  -- Destructor ( There is no need to override this method anytime, use
  --              Finalize() instead to dealloc memory )
  ----------------------------------------------------------------------
  procedure Free(Self : access Ops_Class) is
    Dummy : Ops_Class_At;
    tmp : System.Atomic_Counters.Atomic_Unsigned;
  begin
    Dummy := Ops_Class_At(Self);
    if Dummy /= null then
      System.Atomic_Counters.Decrement(Count);
      -- The count variable may be changed by some one else before we read it below.
      -- For our purpose we don't care since the count itself is protected, it's
      -- only the count in the trace that temporarily may be wrong.
      tmp := Count;
      if TraceRoutine /= null then
        TraceRoutine( Class         => OriginalClassName( Self.all ),
                      CreateStatus  => Dealloc,
                      TotalAllocObj => tmp);
      end if;
      Dealloc(Dummy);
    end if;
  end Free;
                      
  ----------------------------------------------------------------------
  -- Initialize object (Only used to trace allocation of object)
  ----------------------------------------------------------------------
  overriding procedure Initialize(Self : in out Ops_Class) is
    tmp : System.Atomic_Counters.Atomic_Unsigned;
  begin
    System.Atomic_Counters.Increment(Count);
    -- The count variable may be changed by some one else before we read it below.
    -- For our purpose we don't care since the count itself is protected, it's
    -- only the count in the trace that temporarily may be wrong.
    tmp := Count;
    if TraceRoutine /= null then
      TraceRoutine( Class         => OriginalClassName( Self ),
                    CreateStatus  => Alloc,
                    TotalAllocObj => tmp);
    end if;
  end Initialize;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function Copy( Str : String ) return String_At is
    Result : String_At := new String(1..Str'Length);
  begin
    Result.all := Str;
    return Result;
  end Copy;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function Copy( Str : String_At ) return String_At is
  begin
    if Str /= null then
      return Copy( Str.all );
    else
      return null;
    end if;
  end Copy;
  
  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure Replace( Str_At : in out String_At; Str : String ) is
  begin
    if Str_At /= null then
      Dispose(Str_At);
    end if;
    Str_At := Copy(Str);
  end Replace;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure Replace( Str_At : in out String_At; Str : String_At ) is
  begin
    if Str_At /= null then
      Dispose(Str_At);
    end if;
    Str_At := Copy(Str);
  end Replace;
  
  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure Clear( Arr : in out String_Arr ) is
  begin
    for i in Arr'Range loop
      Dispose(Arr(i));
    end loop;
  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  procedure Clear( Arr : in out String_Arr_At ) is
  begin
    if Arr /= null then
      for i in Arr'Range loop
        Dispose(Arr(i));
      end loop;
    end if;
  end;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function Time2F64( Time : Ada.Real_Time.Time ) return Float64 is
    Seconds   : Ada.Real_Time.Seconds_Count;
    TimeSpan  : Ada.Real_Time.Time_Span;
    Dur       : Duration;
  begin
    Ada.Real_Time.Split (Time, Seconds, TimeSpan);
    
    -- Duration since last second
    Dur := Ada.Real_Time.To_Duration(TimeSpan);
    
    return Float64(Dur) + Float64(Seconds);
  end Time2F64;

  --------------------------------------------------------------------------
  --
  --------------------------------------------------------------------------
  function GetTimeInMs return TimeMs_T is
    Diff : Float64;
  begin 
    Diff := 1000.0 * (Time2F64( Ada.Real_Time.Clock ) - StartTime);
    return TimeMs_T(Diff);
  end GetTimeInMs;

  --------------------------------------------------------------------------
  -- Debug
  --------------------------------------------------------------------------
  procedure Trace( NameStr, ValueStr : String ) is
  begin
    if TraceProc /= null then
      TraceProc.all( NameStr, ValueStr );
    else
      Ada.Text_IO.Put_Line( "[ " & NameStr & " ]  " & ValueStr);
    end if;
  end Trace;

  --------------------------------------------------------------------------
  -- Debug
  --------------------------------------------------------------------------
  procedure Trace( NameStr, ValueStr : String; E : Ada.Exceptions.Exception_Occurrence ) is
  begin
    Trace( NameStr, ValueStr & " " & Ada.Exceptions.Exception_Name( E ) & 
                               " " & Ada.Exceptions.Exception_Information( E ) );
  end Trace;

  --------------------------------------------------------------------------
  -- Debug
  --------------------------------------------------------------------------
  function ToString(Ptr : Byte_Arr_At) return String is
  begin
--      if Ptr'Size = UInt32'Size then
--        declare
--          function Conv is new Ada.Unchecked_Conversion(Byte_Arr_At, UInt32);
--          Value : UInt32 := Conv(Ptr);
--        begin
--          return UInt32'Image(Value);
--        end;
--      else
--      declare
--        function Conv is new Ada.Unchecked_Conversion(Byte_Arr_At, UInt64);
--        Value : UInt64 := Conv(Ptr);
--      begin
        return "TBD"; -- this does not give expected result UInt64'Image(Value);
--      end;
--      end if;
  end ToString;
  
  
begin
  StartTime := Time2F64( Ada.Real_Time.Clock );
end Ops_Pa;

