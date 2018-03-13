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

package body Ops_Pa.Transport_Pa.SendDataHandler_Pa.Mc_Pa is

  function Create(topic : Topic_Class_At; localInterface : String; ttl : Integer; Reporter : ErrorService_Class_At) return McSendDataHandler_Class_At is
     Self : McSendDataHandler_Class_At := null;
  begin
    Self := new McSendDataHandler_Class;
    InitInstance( Self.all, topic, localInterface, ttl, Reporter );
    return Self;
  exception
    when others =>
      Free(Self);
      raise;
  end Create;

  overriding function sendData( Self : in out McSendDataHandler_Class; buf : Byte_Arr_At; bufSize : Integer; topic : Topic_Class_At) return Boolean is
  begin
    return Self.Sender.sendTo(buf, bufSize, topic.DomainAddress, Integer(topic.Port));
  end;

  procedure InitInstance( Self : in out McSendDataHandler_Class;
                          topic : Topic_Class_At;
                          localInterface : String;
                          ttl : Integer;
                          Reporter : ErrorService_Class_At ) is
  begin
    InitInstance( SendDataHandler_Class(Self) );
    Self.Sender := createMCSender(localInterface, ttl, topic.OutSocketBufferSize);
    Self.Sender.SetErrorService( Reporter );
  end;

  overriding procedure Finalize( Self : in out McSendDataHandler_Class ) is
  begin
    declare
      S : Ops_Pa.Mutex_Pa.Scope_Lock(Self.Mutex'Access);
    begin
      Free(Self.Sender);
    exception
      when others =>
        null;
    end;

    Finalize( SendDataHandler_Class(Self) );
  end;

end Ops_Pa.Transport_Pa.SendDataHandler_Pa.Mc_Pa;

