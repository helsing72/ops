
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

