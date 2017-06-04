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

package Com_Signal_Pa is

  type Event_T is mod 2**32;
  for Event_T'Size use 32;
  
  NoEvent_C   : constant Event_T := 16#0000_0000#;   
  Event1_C    : constant Event_T := 16#0000_0001#;
  Event2_C    : constant Event_T := 16#0000_0002#;
  Event3_C    : constant Event_T := 16#0000_0004#;
  Event4_C    : constant Event_T := 16#0000_0008#;
          
  protected type Signal_T is

    -- Signal one or more events
    procedure Signal(Events : in Event_T);
    
    -- Wait on any event(s) to be signaled
    entry WaitForAny(Events : out Event_T);

    -- Get and clear current signaled events, if any
    procedure Get(Events : out Event_T);
    
  private
    SignaledEvents : Event_T := NoEvent_C;
  end Signal_T;
  
  type Signal_T_At is access all Signal_T;
        
end Com_Signal_Pa;

