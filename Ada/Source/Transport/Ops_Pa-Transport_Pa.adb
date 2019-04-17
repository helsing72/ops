--
-- Copyright (C) 2016-2019 Lennart Andersson.
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

package body Ops_Pa.Transport_Pa is

  function ErrorService( Self : Transport_Class ) return ErrorService_Class_At is
  begin
    return Self.ErrorService;
  end;

  procedure SetErrorService( Self : in out Transport_Class; es : ErrorService_Class_At ) is
  begin
    Self.ErrorService := es;
  end;

  function LastErrorCode( Self : Transport_Class ) return Integer is
  begin
    return Self.LastErrorCode;
  end;

end Ops_Pa.Transport_Pa;
