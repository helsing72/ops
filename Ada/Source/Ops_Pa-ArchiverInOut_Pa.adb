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

with Ada.Unchecked_Deallocation;

package body Ops_Pa.ArchiverInOut_Pa is

  procedure SetTypesString( Self : in out Serializable_Class; types : String ) is
  begin
    null;
  end;

  procedure SetTypesString( Self : in out ArchiverInOut_Class; obj : Serializable_Class_At; types : String) is
  begin
    obj.all.SetTypesString( types );
  end;

  procedure InoutFixArr(archiver : ArchiverInOut_Class_At; name : string; value : in out Item_Arr) is
    num : Integer;
  begin
    num := archiver.beginList(name, Integer(value'Length));
    if num /= value'Length then
      raise Illegal_Array_Length;
    end if;

    for i in value'Range loop
      declare
        tmp : Item_At := value(i)'Unchecked_Access;
      begin
        archiver.Inout(name, Serializable_Class_At(tmp));
      end;
    end loop;

    archiver.endList(name);
  end;

  procedure inoutdynarr(archiver : ArchiverInOut_Class_At; name : string; value : in out Item_Arr_At) is

    procedure Dispose is new Ada.Unchecked_Deallocation( Item_Arr, Item_Arr_At );

    num : Integer := 0;
  begin
    if value /= null then
      num := Integer(value.all'Length);
    end if;
    num := archiver.beginList(name, num);

    if not archiver.IsOut then
      Dispose(value);

      if num > 0 then
        -- Create new array
        value := new Item_Arr(0..Integer(num-1));
      end if;
    end if;

    if value /= null then
      for i in value.all'Range loop
        declare
          tmp : Item_At := value.all(i)'Unchecked_Access;
        begin
          archiver.Inout(name, Serializable_Class_At(tmp));
        end;
      end loop;
    end if;

    archiver.endList(name);
  end;

  procedure InoutFixArr2(archiver : ArchiverInOut_Class_At; name : string; value : in out Item_At_Arr) is
    num : Integer;
  begin
    num := archiver.beginList(name, Integer(value'Length));
    if num /= value'Length then
      raise Illegal_Array_Length;
    end if;

    for i in value'Range loop
      if archiver.IsOut then
        if value(i) = null then
          raise Null_Object_In_Array;
        end if;
      else
        if value(i) /= null then
          Free(value(i));
        end if;
      end if;
      value(i) := Item_At(archiver.Inout2(name, Serializable_Class_At(value(i))));
    end loop;

    archiver.endList(name);
  end;

  procedure inoutdynarr2(archiver : ArchiverInOut_Class_At; name : string; value : in out Item_At_Arr_At) is

    procedure Dispose is new Ada.Unchecked_Deallocation( Item_At_Arr, Item_At_Arr_At );

    num : Integer := 0;
  begin
    if value /= null then
      num := Integer(value.all'Length);
    end if;
    num := archiver.beginList(name, num);

    if not archiver.IsOut then
      -- First free ev existing objects in the array
      if value /= null then
        for i in value.all'Range loop
          if value(i) /= null then
            Free(value(i));
          end if;
        end loop;
        Dispose(value);
      end if;

      if num > 0 then
        -- Create new array
        value := new Item_At_Arr(0..Integer(num-1));

        for i in value.all'Range loop
          value.all(i) := Item_At(archiver.Inout(name, Serializable_Class_At(value.all(i)), i));
        end loop;
      end if;
    else

      if num > 0 then
        for i in value.all'Range loop
          if value(i) = null then
            raise Null_Object_In_Array;
          end if;
          archiver.Inout(name, Serializable_Class_At(value.all(i)));
        end loop;
      end if;
    end if;

    archiver.endList(name);
  end;

end Ops_Pa.ArchiverInOut_Pa;

