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

with Ops_Pa.Transport_Pa.Sender_Pa.Udp_Pa,
     Ops_Pa.Transport_Pa.Sender_Pa.TCPServer_Pa;

package body Ops_Pa.Transport_Pa.Sender_Pa is

  -- --------------------------------------------------------------------------

  function createMCSender(localInterface : string := "0.0.0.0"; ttl : Integer := 1; outSocketBufferSize : Int64 := 16000000) return Sender_Class_At is
  begin
    return Sender_Class_At(Ops_Pa.Transport_Pa.Sender_Pa.Udp_Pa.Create(localInterface, ttl, outSocketBufferSize, True));
  end;

  function createUDPSender(localInterface : string := "0.0.0.0"; ttl : Integer := 1; outSocketBufferSize : Int64 := 16000000) return Sender_Class_At is
  begin
    return Sender_Class_At(Ops_Pa.Transport_Pa.Sender_Pa.Udp_Pa.Create(localInterface, ttl, outSocketBufferSize, False));
  end;

  function createTCPServer(ip : string; port : Integer; outSocketBufferSize : Int64 := 16000000) return Sender_Class_At is
  begin
   return Sender_Class_At(Ops_Pa.Transport_Pa.Sender_Pa.TCPServer_Pa.Create(ip, port, outSocketBufferSize));
  end;

end Ops_Pa.Transport_Pa.Sender_Pa;

