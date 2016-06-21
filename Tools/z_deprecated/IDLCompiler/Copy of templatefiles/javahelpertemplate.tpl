//Auto generated OPS-code. DO NOT MODIFY!
package __packageName;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.DataInputStream;
import java.io.DataOutputStream;
import java.io.IOException;
import ops.OPSObject;
import ops.OPSObjectHelper;

//Auto generated helper class for __className
public class __classNameHelper extends OPSObjectHelper 
{
    
    public __classNameHelper()
    {
        
        
    }

    //Auto generated serialization code for a __className
    public byte[] serialize(OPSObject o) throws IOException
    {
 __serialize
    }

    //Auto generated deserialization code for a __className
    public OPSObject deserialize(byte[] __b) throws IOException
    {
__deserialize
    }
    
    //Auto generated code for getting the current size of a __className
    public int getSize(OPSObject o)
    {
__size
        
    }

    //Auto generated code for getting the typeID of a __className
    public String getTypeID()
    {
        return "__packageName.__className";
    }

    public static __className copy(__className o) 
    {
        try
        {
            __classNameHelper helper = new __classNameHelper();
            byte[] bytes = helper.serialize(o);
            __className c = (__className)helper.deserialize(bytes);
            return c; 
        }
        catch(IOException e)
        {
            return null;
        }
        
    }

}