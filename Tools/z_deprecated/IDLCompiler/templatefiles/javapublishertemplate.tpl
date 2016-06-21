//Auto generated OPS-code. !DO NOT MODIFY!

package __packageName;

import ops.CommException;
import ops.Publisher;
import ops.OPSObject;
import ops.Topic;
import ops.AckData;


public class __classNamePublisher extends Publisher 
{
    

    public __classNamePublisher(Topic<__className> t) 
    {
        super(t);
        setObjectHelper(new __classNameHelper());

    }
    public void write(__className o) 
    {
        super.write(o);
    }
    public AckData writeReliable(__className o, String destinationIdentity) throws CommException 
    {
        return super.writeReliable(o, destinationIdentity);
    }

    
}