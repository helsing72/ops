--
-- Copyright (C) 2016-2017 Lennart Andersson.
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

package body Ops_Pa.FilterQoSPolicy_Pa.KeyFilterQoSPolicy_Pa is

  protected body KeyStorage_T is

    procedure Init( Max : Integer ) is
    begin
      Max_Keys := Max;
      Keys := new String_Arr(0..Max_Keys-1);
    end;

    procedure Dispose is
    begin
      Clear;
      Dispose(Keys);
    end;

    procedure Add( Key : String ) is
      Found : Boolean := False;
    begin
      if Key /= "" then
        -- Check if already there
        for i in Keys'First .. Keys'First + Num_Keys - 1 loop
          if Keys(i).all = Key then
            Found := True;
          end if;
        end loop;

        if Found then
          return;
        end if;

        -- check if room
        if Num_Keys >= Max_Keys then
          raise TooManyKeys;
        end if;

        -- if not, add it
        Keys(Num_Keys) := new String'(Key);
        Num_Keys := Num_Keys + 1;
      end if;
    end;

    procedure Clear is
    begin
      for i in Keys'Range loop
        if Keys(i) /= null then
          Dispose(Keys(i));
          Keys(i) := null;
        end if;
      end loop;
      Num_Keys := 0;
    end;

    -- Returns True if matches filter
    function Check( o : OpsObject_Class_At ) return Boolean is
    begin
      -- An empty key filter is the same as no filter, i.e. keep object
      if Num_Keys = 0 then
        return True;
      end if;

      for i in Keys'First .. Keys'First + Num_Keys - 1 loop
        if Keys(i).all = o.Key then
          return True;  -- match, so keep object
        end if;
      end loop;

      return False;  -- no match, skip this object
    end;

  end KeyStorage_T;


  -- Creates a filter that as default lets all objects thru.
  -- If a key is given, it must match for objects to come thru.
  function Create( Max_Keys : Integer := 10; Key : String := "" ) return KeyFilterQoSPolicy_Class_At is
     Self : KeyFilterQoSPolicy_Class_At := null;
  begin
    Self := new KeyFilterQoSPolicy_Class;
    InitInstance( Self.all, Max_Keys, Key );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  procedure InitInstance( Self : in out KeyFilterQoSPolicy_Class;  Max_Keys : Integer; Key : String ) is
  begin
    Self.Keys.Init( Max_Keys );
    Self.Keys.Add( Key );
  end;

  procedure Finalize( Self : in out KeyFilterQoSPolicy_Class ) is
  begin
    Self.Keys.Dispose;
  end;

  -- Add more keys, if any key matches, the object comes thru
  procedure AddKey( Self : in out KeyFilterQoSPolicy_Class; Key : String ) is
  begin
    Self.Keys.Add( Key );
  end;

  -- Clear all keys making an empty filter that lets all objects thru
  procedure Clear( Self : in out KeyFilterQoSPolicy_Class) is
  begin
    Self.Keys.Clear;
  end;

  -- Returning false from a filter indicates that this data sample (OPSObject)
  -- shall not be propagated to the application layer.
  overriding function ApplyFilter( Self : KeyFilterQoSPolicy_Class; o : OpsObject_Class_At) return Boolean is
  begin
    return Self.Keys.Check( o );
  end;

end Ops_Pa.FilterQoSPolicy_Pa.KeyFilterQoSPolicy_Pa;

