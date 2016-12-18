--
-- Copyright (C) 2016 Lennart Andersson.
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

package body Ops_Pa.SerializableFactory_Pa.SerializableCompositeFactory_Pa is

  -- Constructors
  function Create return SerializableCompositeFactory_Class_At is
    Self : SerializableCompositeFactory_Class_At := null;
  begin
    Self := new SerializableCompositeFactory_Class;
    InitInstance( Self.all );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  function Equal(Left, Right : SerializableFactory_Class_At) return Boolean is
  begin
    return Left = Right;
  end;

  procedure Add( Self : in out SerializableCompositeFactory_Class; o : SerializableFactory_Class_At ) is
  begin
    Self.ChildFactories.Append(o);
  end;

  procedure Remove( Self : in out SerializableCompositeFactory_Class; o : SerializableFactory_Class_At ) is
    Idx : MyVector_Pa.Extended_Index;
  begin
    Idx := Self.ChildFactories.Find_Index(o);
    if Idx /= MyVector_Pa.No_Index then
      -- Remove from list without freeing object
      Self.ChildFactories.Delete(Idx);
    end if;
  end;

  -- Create a serializable class instance from given type
  function Make( Self : SerializableCompositeFactory_Class; types : string) return Serializable_Class_At is
    Result : Serializable_Class_At;
  begin
    for i in Self.ChildFactories.First_Index .. Self.ChildFactories.Last_Index loop
      Result := Self.ChildFactories.Element(i).Make( types );
      if Result /= null then
        return Result;
      end if;
    end loop;
    return null;
  end;

  procedure InitInstance( Self : in out SerializableCompositeFactory_Class ) is
  begin
    null;
  end;

  procedure Finalize( Self : in out SerializableCompositeFactory_Class ) is
  begin
    -- Free all objects in the list
    for i in Self.ChildFactories.First_Index .. Self.ChildFactories.Last_Index loop
      if Self.ChildFactories.Element(i) /= null then
        Free(Self.ChildFactories.Element(i));
      end if;
    end loop;
    Self.ChildFactories.Clear;
  end;

end Ops_Pa.SerializableFactory_Pa.SerializableCompositeFactory_Pa;
