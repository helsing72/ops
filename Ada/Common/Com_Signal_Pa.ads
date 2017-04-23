
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

