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

package body Com_Signal_Pa is
    
  protected body Signal_T is
  
    procedure Signal(Events : in Event_T) is
    begin
      SignaledEvents := SignaledEvents or Events;
    end;
    
    entry WaitForAny(Events : out Event_T) when SignaledEvents /= NoEvent_C is
    begin
      Events         := SignaledEvents;
      SignaledEvents := NoEvent_C;
    end;            
    
    -- Get current signaled events, if any
    procedure Get(Events : out Event_T) is
    begin
      Events         := SignaledEvents;
      SignaledEvents := NoEvent_C;
    end;
    
  end Signal_T;            
  
end Com_Signal_Pa;

