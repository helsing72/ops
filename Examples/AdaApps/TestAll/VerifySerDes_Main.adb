--
-- Copyright (C) 2017 Lennart Andersson.
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

with Ada.Text_Io;
with Ctv;
with Com_Base_Abs_Pa;
with VerifySerDes_Pa;

use Ada.Text_IO;
use Ctv;
use VerifySerDes_Pa;

procedure VerifySerDes_Main is
begin
  Put_Line("Debug: Count= " & Integer32'Image(Com_Base_Abs_Pa.NumActiveObjects));
  VerifySerDes;
  Put_Line("Debug: Count= " & Integer32'Image(Com_Base_Abs_Pa.NumActiveObjects));
end;

